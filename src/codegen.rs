use crate::ast::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue, ValueKind};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::path::Path;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, (PointerValue<'ctx>, Type)>,
    printf_fn: Option<FunctionValue<'ctx>>,
    malloc_fn: Option<FunctionValue<'ctx>>,
    /// Compiled function values
    functions: HashMap<String, FunctionValue<'ctx>>,
    /// Class struct types: class_name -> (struct_type, field_names_in_order)
    class_types: HashMap<String, (inkwell::types::StructType<'ctx>, Vec<String>)>,
    /// Class field types: class_name -> { field_name -> Type }
    class_field_types: HashMap<String, HashMap<String, Type>>,
    /// Class method functions: class_name -> { method_name -> FunctionValue }
    class_methods: HashMap<String, HashMap<String, FunctionValue<'ctx>>>,
    /// Class constructor functions: class_name -> FunctionValue
    class_constructors: HashMap<String, FunctionValue<'ctx>>,
    /// Current self pointer (when compiling methods/constructors)
    current_self: Option<PointerValue<'ctx>>,
    /// Current class name (when compiling methods/constructors)
    current_class_name: Option<String>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            variables: HashMap::new(),
            printf_fn: None,
            malloc_fn: None,
            functions: HashMap::new(),
            class_types: HashMap::new(),
            class_field_types: HashMap::new(),
            class_methods: HashMap::new(),
            class_constructors: HashMap::new(),
            current_self: None,
            current_class_name: None,
        }
    }

    pub fn compile(&mut self, program: &Program) -> Result<(), String> {
        self.declare_printf();
        self.declare_malloc();

        // First pass: declare class struct types
        for class in &program.classes {
            self.declare_class_type(class)?;
        }

        // Second pass: declare all functions (forward declarations)
        for func in &program.functions {
            self.declare_function(func)?;
        }

        // Third pass: declare class methods and constructors
        for class in &program.classes {
            self.declare_class_methods(class)?;
        }

        // Fourth pass: compile function bodies
        for func in &program.functions {
            self.compile_function(func)?;
        }

        // Fifth pass: compile class method/constructor bodies
        for class in &program.classes {
            self.compile_class_bodies(class)?;
        }

        // Finally: create main function with top-level statements
        self.create_main_function(program)?;
        Ok(())
    }

    fn declare_printf(&mut self) {
        let i32_type = self.context.i32_type();
        let i8_ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let printf_type = i32_type.fn_type(&[i8_ptr_type.into()], true);
        self.printf_fn = Some(self.module.add_function("printf", printf_type, None));
    }

    fn declare_malloc(&mut self) {
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let i64_type = self.context.i64_type();
        let malloc_type = ptr_type.fn_type(&[i64_type.into()], false);
        self.malloc_fn = Some(self.module.add_function("malloc", malloc_type, None));
    }

    // ========== Function Compilation ==========

    fn declare_function(&mut self, func: &FunctionDef) -> Result<(), String> {
        let return_type = &func.return_type;
        let param_types: Vec<BasicTypeEnum<'ctx>> = func
            .parameters
            .iter()
            .map(|p| self.get_llvm_type(&p.param_type))
            .collect();

        let param_meta: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
            param_types.iter().map(|t| (*t).into()).collect();

        let fn_type = if *return_type == Type::Void {
            self.context.void_type().fn_type(&param_meta, false)
        } else {
            let ret_llvm = self.get_llvm_type(return_type);
            ret_llvm.fn_type(&param_meta, false)
        };

        let fn_value = self.module.add_function(&func.name, fn_type, None);
        self.functions.insert(func.name.clone(), fn_value);
        Ok(())
    }

    fn compile_function(&mut self, func: &FunctionDef) -> Result<(), String> {
        let fn_value = *self
            .functions
            .get(&func.name)
            .ok_or_else(|| format!("Function {} not declared", func.name))?;

        let entry = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry);

        // Save and clear current variables scope
        let saved_vars = std::mem::take(&mut self.variables);

        // Bind parameters to allocas
        for (i, param) in func.parameters.iter().enumerate() {
            let param_val = fn_value.get_nth_param(i as u32).unwrap();
            let alloca = self
                .builder
                .build_alloca(self.get_llvm_type(&param.param_type), &param.name)
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(alloca, param_val)
                .map_err(|e| e.to_string())?;
            self.variables
                .insert(param.name.clone(), (alloca, param.param_type.clone()));
        }

        // Compile body
        for stmt in &func.body {
            self.compile_statement(stmt)?;
        }

        // If void function, add implicit return
        if func.return_type == Type::Void {
            // Check if the current block doesn't already have a terminator
            if self
                .builder
                .get_insert_block()
                .unwrap()
                .get_terminator()
                .is_none()
            {
                self.builder
                    .build_return(None)
                    .map_err(|e| e.to_string())?;
            }
        }

        // Restore variables
        self.variables = saved_vars;
        Ok(())
    }

    // ========== Class Compilation ==========

    fn declare_class_type(&mut self, class: &ClassDef) -> Result<(), String> {
        // Collect all fields (including inherited)
        let mut field_names: Vec<String> = Vec::new();
        let mut field_llvm_types: Vec<BasicTypeEnum<'ctx>> = Vec::new();
        let mut field_type_map: HashMap<String, Type> = HashMap::new();

        // If there's a parent, include its fields first
        if let Some(parent_name) = &class.parent {
            if let Some((_, parent_fields)) = self.class_types.get(parent_name) {
                let parent_field_types = self.class_field_types.get(parent_name).unwrap();
                for field_name in parent_fields {
                    let ft = parent_field_types.get(field_name).unwrap().clone();
                    field_names.push(field_name.clone());
                    field_llvm_types.push(self.get_llvm_type(&ft));
                    field_type_map.insert(field_name.clone(), ft);
                }
            }
        }

        // Add own properties
        for prop in &class.properties {
            if !field_names.contains(&prop.name) {
                field_names.push(prop.name.clone());
                field_llvm_types.push(self.get_llvm_type(&prop.prop_type));
                field_type_map.insert(prop.name.clone(), prop.prop_type.clone());
            }
        }

        let struct_type = self
            .context
            .opaque_struct_type(&format!("class.{}", class.name));
        struct_type.set_body(&field_llvm_types, false);

        self.class_types
            .insert(class.name.clone(), (struct_type, field_names));
        self.class_field_types
            .insert(class.name.clone(), field_type_map);

        Ok(())
    }

    fn declare_class_methods(&mut self, class: &ClassDef) -> Result<(), String> {
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let mut methods_map: HashMap<String, FunctionValue<'ctx>> = HashMap::new();

        // Inherit parent methods
        if let Some(parent_name) = &class.parent {
            if let Some(parent_methods) = self.class_methods.get(parent_name) {
                methods_map.extend(parent_methods.clone());
            }
        }

        // Declare constructor
        if let Some(constructor) = &class.constructor {
            let mut param_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
                vec![ptr_type.into()]; // self pointer
            for param in &constructor.parameters {
                param_types.push(self.get_llvm_type(&param.param_type).into());
            }
            let ctor_type = self.context.void_type().fn_type(&param_types, false);
            let ctor_name = format!("{}_create", class.name);
            let ctor_fn = self.module.add_function(&ctor_name, ctor_type, None);
            self.class_constructors
                .insert(class.name.clone(), ctor_fn);
        }

        // Declare methods
        for method in &class.methods {
            let mut param_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
                vec![ptr_type.into()]; // self pointer
            for param in &method.parameters {
                param_types.push(self.get_llvm_type(&param.param_type).into());
            }

            let fn_type = if method.return_type == Type::Void {
                self.context.void_type().fn_type(&param_types, false)
            } else {
                self.get_llvm_type(&method.return_type)
                    .fn_type(&param_types, false)
            };

            let method_name = format!("{}_{}", class.name, method.name);
            let method_fn = self.module.add_function(&method_name, fn_type, None);
            methods_map.insert(method.name.clone(), method_fn);
        }

        self.class_methods
            .insert(class.name.clone(), methods_map);
        Ok(())
    }

    fn compile_class_bodies(&mut self, class: &ClassDef) -> Result<(), String> {
        let (struct_type, field_names) = self.class_types.get(&class.name).unwrap().clone();
        let field_type_map = self.class_field_types.get(&class.name).unwrap().clone();

        // Compile constructor
        if let Some(constructor) = &class.constructor {
            let ctor_fn = *self.class_constructors.get(&class.name).unwrap();
            let entry = self.context.append_basic_block(ctor_fn, "entry");
            self.builder.position_at_end(entry);

            let saved_vars = std::mem::take(&mut self.variables);
            let saved_self = self.current_self.take();
            let saved_class = self.current_class_name.take();

            let self_ptr = ctor_fn.get_nth_param(0).unwrap().into_pointer_value();
            self.current_self = Some(self_ptr);
            self.current_class_name = Some(class.name.clone());

            // Bind properties as variables (pointing into struct fields)
            for (idx, fname) in field_names.iter().enumerate() {
                let ftype = field_type_map.get(fname).unwrap().clone();
                let field_ptr = self
                    .builder
                    .build_struct_gep(struct_type, self_ptr, idx as u32, fname)
                    .map_err(|e| format!("GEP error: {:?}", e))?;
                self.variables
                    .insert(fname.clone(), (field_ptr, ftype));
            }

            // Bind constructor parameters
            for (i, param) in constructor.parameters.iter().enumerate() {
                let param_val = ctor_fn.get_nth_param((i + 1) as u32).unwrap();
                let alloca = self
                    .builder
                    .build_alloca(self.get_llvm_type(&param.param_type), &param.name)
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(alloca, param_val)
                    .map_err(|e| e.to_string())?;
                self.variables
                    .insert(param.name.clone(), (alloca, param.param_type.clone()));
            }

            for stmt in &constructor.body {
                self.compile_statement(stmt)?;
            }

            self.builder
                .build_return(None)
                .map_err(|e| e.to_string())?;

            self.variables = saved_vars;
            self.current_self = saved_self;
            self.current_class_name = saved_class;
        }

        // Compile methods
        for method in &class.methods {
            let method_fn = *self
                .class_methods
                .get(&class.name)
                .unwrap()
                .get(&method.name)
                .unwrap();

            let entry = self.context.append_basic_block(method_fn, "entry");
            self.builder.position_at_end(entry);

            let saved_vars = std::mem::take(&mut self.variables);
            let saved_self = self.current_self.take();
            let saved_class = self.current_class_name.take();

            let self_ptr = method_fn.get_nth_param(0).unwrap().into_pointer_value();
            self.current_self = Some(self_ptr);
            self.current_class_name = Some(class.name.clone());

            // Bind properties as variables
            for (idx, fname) in field_names.iter().enumerate() {
                let ftype = field_type_map.get(fname).unwrap().clone();
                let field_ptr = self
                    .builder
                    .build_struct_gep(struct_type, self_ptr, idx as u32, fname)
                    .map_err(|e| format!("GEP error: {:?}", e))?;
                self.variables
                    .insert(fname.clone(), (field_ptr, ftype));
            }

            // Bind method parameters
            for (i, param) in method.parameters.iter().enumerate() {
                let param_val = method_fn.get_nth_param((i + 1) as u32).unwrap();
                let alloca = self
                    .builder
                    .build_alloca(self.get_llvm_type(&param.param_type), &param.name)
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(alloca, param_val)
                    .map_err(|e| e.to_string())?;
                self.variables
                    .insert(param.name.clone(), (alloca, param.param_type.clone()));
            }

            for stmt in &method.body {
                self.compile_statement(stmt)?;
            }

            // Implicit return for void methods
            if method.return_type == Type::Void {
                if self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    self.builder
                        .build_return(None)
                        .map_err(|e| e.to_string())?;
                }
            }

            self.variables = saved_vars;
            self.current_self = saved_self;
            self.current_class_name = saved_class;
        }

        Ok(())
    }

    fn create_main_function(&mut self, program: &Program) -> Result<(), String> {
        let i32_type = self.context.i32_type();
        let main_type = i32_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_type, None);

        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        for stmt in &program.statements {
            self.compile_statement(stmt)?;
        }

        self.builder
            .build_return(Some(&i32_type.const_int(0, false)))
            .map_err(|e| e.to_string())?;

        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Statement) -> Result<(), String> {
        match stmt {
            Statement::VariableDecl {
                name,
                var_type,
                value,
            } => {
                let llvm_type = self.get_llvm_type(var_type);
                let alloca = self
                    .builder
                    .build_alloca(llvm_type, name)
                    .map_err(|e| e.to_string())?;

                let value = self.compile_expression(value)?;
                self.builder
                    .build_store(alloca, value)
                    .map_err(|e| e.to_string())?;

                self.variables
                    .insert(name.clone(), (alloca, var_type.clone()));
            }

            Statement::CompoundAssignment { name, op, value } => {
                let (ptr, var_type) = self
                    .variables
                    .get(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?
                    .clone();

                let current = self
                    .builder
                    .build_load(self.get_llvm_type(&var_type), ptr, "load")
                    .map_err(|e| e.to_string())?;

                let operand = self.compile_expression(value)?;
                let result = self.compile_binary_op(op, current, operand, &var_type)?;

                self.builder
                    .build_store(ptr, result)
                    .map_err(|e| e.to_string())?;
            }

            Statement::Output(expr) => {
                let value = self.compile_expression(expr)?;
                self.emit_print(value, expr)?;
            }

            Statement::If {
                condition,
                then_block,
                else_block,
            } => {
                let cond_value = self.compile_expression(condition)?;
                let cond_bool = match cond_value {
                    BasicValueEnum::IntValue(v) => v,
                    _ => return Err("Condition must be boolean".to_string()),
                };

                let function = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();

                let then_bb = self.context.append_basic_block(function, "then");
                let else_bb = self.context.append_basic_block(function, "else");
                let merge_bb = self.context.append_basic_block(function, "merge");

                self.builder
                    .build_conditional_branch(cond_bool, then_bb, else_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(then_bb);
                for stmt in then_block {
                    self.compile_statement(stmt)?;
                }
                self.builder
                    .build_unconditional_branch(merge_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(else_bb);
                if let Some(else_stmts) = else_block {
                    for stmt in else_stmts {
                        self.compile_statement(stmt)?;
                    }
                }
                self.builder
                    .build_unconditional_branch(merge_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(merge_bb);
            }

            Statement::While { condition, body } => {
                let function = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();

                let cond_bb = self.context.append_basic_block(function, "while_cond");
                let body_bb = self.context.append_basic_block(function, "while_body");
                let after_bb = self.context.append_basic_block(function, "while_after");

                self.builder
                    .build_unconditional_branch(cond_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(cond_bb);
                let cond_value = self.compile_expression(condition)?;
                let cond_bool = match cond_value {
                    BasicValueEnum::IntValue(v) => v,
                    _ => return Err("Condition must be boolean".to_string()),
                };
                self.builder
                    .build_conditional_branch(cond_bool, body_bb, after_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(body_bb);
                for stmt in body {
                    self.compile_statement(stmt)?;
                }
                self.builder
                    .build_unconditional_branch(cond_bb)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(after_bb);
            }

            Statement::Assignment { name, value } => {
                let (ptr, _) = self
                    .variables
                    .get(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?
                    .clone();

                let val = self.compile_expression(value)?;
                self.builder
                    .build_store(ptr, val)
                    .map_err(|e| e.to_string())?;
            }

            Statement::ExprStatement(expr) => {
                self.compile_expression(expr)?;
            }

            Statement::Return(expr) => {
                if let Some(return_expr) = expr {
                    let val = self.compile_expression(return_expr)?;
                    self.builder
                        .build_return(Some(&val))
                        .map_err(|e| e.to_string())?;
                } else {
                    self.builder
                        .build_return(None)
                        .map_err(|e| e.to_string())?;
                }
            }

            Statement::PropertyAssignment {
                object,
                property,
                value,
            } => {
                let (obj_ptr, obj_type) = self
                    .variables
                    .get(object)
                    .ok_or_else(|| format!("Undefined variable: {}", object))?
                    .clone();

                let class_name = match &obj_type {
                    Type::Class(name) => name.clone(),
                    _ => return Err(format!("{} is not a class instance", object)),
                };

                let (struct_type, field_names) = self
                    .class_types
                    .get(&class_name)
                    .ok_or_else(|| format!("Unknown class: {}", class_name))?
                    .clone();

                let field_idx = field_names
                    .iter()
                    .position(|n| n == property)
                    .ok_or_else(|| format!("No field {} on {}", property, class_name))?;

                // obj_ptr is a pointer to a pointer (alloca of the heap pointer)
                let heap_ptr = self
                    .builder
                    .build_load(
                        self.context.ptr_type(inkwell::AddressSpace::default()),
                        obj_ptr,
                        "obj_load",
                    )
                    .map_err(|e| e.to_string())?
                    .into_pointer_value();

                let field_ptr = self
                    .builder
                    .build_struct_gep(struct_type, heap_ptr, field_idx as u32, property)
                    .map_err(|e| format!("GEP error: {:?}", e))?;

                let val = self.compile_expression(value)?;
                self.builder
                    .build_store(field_ptr, val)
                    .map_err(|e| e.to_string())?;
            }
        }

        Ok(())
    }

    fn compile_expression(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, String> {
        match expr {
            Expr::IntLiteral(n) => {
                let i64_type = self.context.i64_type();
                Ok(i64_type.const_int(*n as u64, false).into())
            }

            Expr::FloatLiteral(n) => {
                let f64_type = self.context.f64_type();
                Ok(f64_type.const_float(*n).into())
            }

            Expr::StringLiteral(s) => {
                let string_ptr = self
                    .builder
                    .build_global_string_ptr(s, "str")
                    .map_err(|e| e.to_string())?;
                Ok(string_ptr.as_pointer_value().into())
            }

            Expr::BoolLiteral(b) => {
                let i1_type = self.context.bool_type();
                Ok(i1_type.const_int(*b as u64, false).into())
            }

            Expr::Identifier(name) => {
                let (ptr, var_type) = self
                    .variables
                    .get(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?
                    .clone();

                let llvm_type = self.get_llvm_type(&var_type);
                Ok(self
                    .builder
                    .build_load(llvm_type, ptr, name)
                    .map_err(|e| e.to_string())?)
            }

            Expr::BinaryOp { op, left, right } => {
                let left_val = self.compile_expression(left)?;
                let right_val = self.compile_expression(right)?;

                let result_type = self.infer_type(expr);
                self.compile_binary_op(op, left_val, right_val, &result_type)
            }

            Expr::TypeConversion { target_type, expr } => {
                let value = self.compile_expression(expr)?;
                self.compile_type_conversion(value, target_type)
            }

            Expr::UnaryOp { op, operand } => {
                let val = self.compile_expression(operand)?;
                match op {
                    UnaryOp::Negate => match val {
                        BasicValueEnum::IntValue(v) => Ok(self
                            .builder
                            .build_int_neg(v, "neg")
                            .map_err(|e| e.to_string())?
                            .into()),
                        BasicValueEnum::FloatValue(v) => Ok(self
                            .builder
                            .build_float_neg(v, "neg")
                            .map_err(|e| e.to_string())?
                            .into()),
                        _ => Err("Cannot negate this type".to_string()),
                    },
                    UnaryOp::Not => match val {
                        BasicValueEnum::IntValue(v) => Ok(self
                            .builder
                            .build_not(v, "not")
                            .map_err(|e| e.to_string())?
                            .into()),
                        _ => Err("Cannot apply 'not' to this type".to_string()),
                    },
                }
            }

            Expr::ListLiteral(_) => Err("List literals not yet implemented".to_string()),

            Expr::Index { .. } => Err("Index access not yet implemented".to_string()),

            Expr::FunctionCall { name, arguments } => {
                let fn_value = *self
                    .functions
                    .get(name)
                    .ok_or_else(|| format!("Undefined function: {}", name))?;

                let mut args: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> = Vec::new();
                for arg in arguments {
                    args.push(self.compile_expression(arg)?.into());
                }

                let call = self
                    .builder
                    .build_call(fn_value, &args, "call")
                    .map_err(|e| e.to_string())?;

                // If the function returns void, return a dummy value
                match call.try_as_basic_value() {
                    ValueKind::Basic(val) => Ok(val),
                    ValueKind::Instruction(_) => {
                        // Void return - return a dummy i64 0
                        Ok(self.context.i64_type().const_int(0, false).into())
                    }
                }
            }

            Expr::NewObject {
                class_name,
                arguments,
            } => {
                let (struct_type, _) = self
                    .class_types
                    .get(class_name)
                    .ok_or_else(|| format!("Unknown class: {}", class_name))?
                    .clone();

                // Allocate memory on the heap
                let malloc = self.malloc_fn.ok_or("malloc not declared")?;
                let struct_size = struct_type.size_of().unwrap();
                let size_val: BasicValueEnum<'ctx> = struct_size.into();
                let malloc_result = self
                    .builder
                    .build_call(malloc, &[size_val.into()], "obj_alloc")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value();
                let heap_ptr = match malloc_result {
                    ValueKind::Basic(val) => val.into_pointer_value(),
                    ValueKind::Instruction(_) => return Err("malloc returned void".to_string()),
                };

                // Call constructor if it exists
                if let Some(ctor_fn) = self.class_constructors.get(class_name).copied() {
                    let mut args: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> =
                        vec![heap_ptr.into()];
                    for arg in arguments {
                        args.push(self.compile_expression(arg)?.into());
                    }
                    self.builder
                        .build_call(ctor_fn, &args, "ctor_call")
                        .map_err(|e| e.to_string())?;
                }

                Ok(heap_ptr.into())
            }

            Expr::MethodCall {
                object,
                method,
                arguments,
            } => {
                let obj_val = self.compile_expression(object)?;
                let obj_ptr = match obj_val {
                    BasicValueEnum::PointerValue(p) => p,
                    _ => return Err("Expected object pointer for method call".to_string()),
                };

                // Determine the class name from the object expression
                let class_name = self.infer_class_name(object)?;

                let method_fn = *self
                    .class_methods
                    .get(&class_name)
                    .and_then(|m| m.get(method))
                    .ok_or_else(|| format!("No method {} on {}", method, class_name))?;

                let mut args: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> =
                    vec![obj_ptr.into()];
                for arg in arguments {
                    args.push(self.compile_expression(arg)?.into());
                }

                let call = self
                    .builder
                    .build_call(method_fn, &args, "method_call")
                    .map_err(|e| e.to_string())?;

                match call.try_as_basic_value() {
                    ValueKind::Basic(val) => Ok(val),
                    ValueKind::Instruction(_) => {
                        Ok(self.context.i64_type().const_int(0, false).into())
                    }
                }
            }

            Expr::PropertyAccess { object, property } => {
                let obj_val = self.compile_expression(object)?;
                let obj_ptr = match obj_val {
                    BasicValueEnum::PointerValue(p) => p,
                    _ => return Err("Expected object pointer for property access".to_string()),
                };

                let class_name = self.infer_class_name(object)?;

                let (struct_type, field_names) = self
                    .class_types
                    .get(&class_name)
                    .ok_or_else(|| format!("Unknown class: {}", class_name))?
                    .clone();

                let field_idx = field_names
                    .iter()
                    .position(|n| n == property)
                    .ok_or_else(|| format!("No field {} on {}", property, class_name))?;

                let field_type = self
                    .class_field_types
                    .get(&class_name)
                    .and_then(|m| m.get(property))
                    .ok_or_else(|| format!("No field {} on {}", property, class_name))?
                    .clone();

                let field_ptr = self
                    .builder
                    .build_struct_gep(struct_type, obj_ptr, field_idx as u32, property)
                    .map_err(|e| format!("GEP error: {:?}", e))?;

                let llvm_type = self.get_llvm_type(&field_type);
                Ok(self
                    .builder
                    .build_load(llvm_type, field_ptr, &format!("load_{}", property))
                    .map_err(|e| e.to_string())?)
            }
        }
    }

    /// Infer the class name from an expression (for method calls and property access)
    fn infer_class_name(&self, expr: &Expr) -> Result<String, String> {
        match expr {
            Expr::Identifier(name) => {
                let (_, var_type) = self
                    .variables
                    .get(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?;
                match var_type {
                    Type::Class(class_name) => Ok(class_name.clone()),
                    _ => Err(format!("{} is not a class instance", name)),
                }
            }
            _ => Err("Cannot determine class type for expression".to_string()),
        }
    }

    fn compile_binary_op(
        &self,
        op: &BinaryOp,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
        result_type: &Type,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        match (left, right) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                let result = match op {
                    BinaryOp::Add => self
                        .builder
                        .build_int_add(l, r, "add")
                        .map_err(|e| e.to_string())?,
                    BinaryOp::Subtract => self
                        .builder
                        .build_int_sub(l, r, "sub")
                        .map_err(|e| e.to_string())?,
                    BinaryOp::Multiply => self
                        .builder
                        .build_int_mul(l, r, "mul")
                        .map_err(|e| e.to_string())?,
                    BinaryOp::Divide => self
                        .builder
                        .build_int_signed_div(l, r, "div")
                        .map_err(|e| e.to_string())?,
                    BinaryOp::Remainder => self
                        .builder
                        .build_int_signed_rem(l, r, "rem")
                        .map_err(|e| e.to_string())?,
                    BinaryOp::Quotient => self
                        .builder
                        .build_int_signed_div(l, r, "quot")
                        .map_err(|e| e.to_string())?,
                    BinaryOp::Equal => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, l, r, "eq")
                        .map_err(|e| e.to_string())?,
                    BinaryOp::NotEqual => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::NE, l, r, "ne")
                        .map_err(|e| e.to_string())?,
                    BinaryOp::Greater => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SGT, l, r, "gt")
                        .map_err(|e| e.to_string())?,
                    BinaryOp::Less => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SLT, l, r, "lt")
                        .map_err(|e| e.to_string())?,
                    BinaryOp::GreaterEq => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SGE, l, r, "ge")
                        .map_err(|e| e.to_string())?,
                    BinaryOp::LessEq => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SLE, l, r, "le")
                        .map_err(|e| e.to_string())?,
                    BinaryOp::And => self
                        .builder
                        .build_and(l, r, "and")
                        .map_err(|e| e.to_string())?,
                    BinaryOp::Or => self
                        .builder
                        .build_or(l, r, "or")
                        .map_err(|e| e.to_string())?,
                };
                Ok(result.into())
            }
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                let result = match op {
                    BinaryOp::Add => self
                        .builder
                        .build_float_add(l, r, "add")
                        .map_err(|e| e.to_string())?
                        .into(),
                    BinaryOp::Subtract => self
                        .builder
                        .build_float_sub(l, r, "sub")
                        .map_err(|e| e.to_string())?
                        .into(),
                    BinaryOp::Multiply => self
                        .builder
                        .build_float_mul(l, r, "mul")
                        .map_err(|e| e.to_string())?
                        .into(),
                    BinaryOp::Divide => self
                        .builder
                        .build_float_div(l, r, "div")
                        .map_err(|e| e.to_string())?
                        .into(),
                    BinaryOp::Greater => self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::OGT, l, r, "gt")
                        .map_err(|e| e.to_string())?
                        .into(),
                    BinaryOp::Less => self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::OLT, l, r, "lt")
                        .map_err(|e| e.to_string())?
                        .into(),
                    _ => return Err(format!("Unsupported float operation: {:?}", op)),
                };
                Ok(result)
            }
            (BasicValueEnum::IntValue(l), BasicValueEnum::FloatValue(r)) => {
                let l_float = self
                    .builder
                    .build_signed_int_to_float(l, self.context.f64_type(), "itof")
                    .map_err(|e| e.to_string())?;
                self.compile_binary_op(op, l_float.into(), r.into(), result_type)
            }
            (BasicValueEnum::FloatValue(l), BasicValueEnum::IntValue(r)) => {
                let r_float = self
                    .builder
                    .build_signed_int_to_float(r, self.context.f64_type(), "itof")
                    .map_err(|e| e.to_string())?;
                self.compile_binary_op(op, l.into(), r_float.into(), result_type)
            }
            _ => Err("Unsupported operand types for binary operation".to_string()),
        }
    }

    fn compile_type_conversion(
        &self,
        value: BasicValueEnum<'ctx>,
        target_type: &Type,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        match (value, target_type) {
            (BasicValueEnum::IntValue(v), Type::Float) => Ok(self
                .builder
                .build_signed_int_to_float(v, self.context.f64_type(), "itof")
                .map_err(|e| e.to_string())?
                .into()),
            (BasicValueEnum::FloatValue(v), Type::Int) => Ok(self
                .builder
                .build_float_to_signed_int(v, self.context.i64_type(), "ftoi")
                .map_err(|e| e.to_string())?
                .into()),
            _ => Err(format!("Unsupported type conversion to {:?}", target_type)),
        }
    }

    fn emit_print(&self, value: BasicValueEnum<'ctx>, expr: &Expr) -> Result<(), String> {
        let printf = self.printf_fn.ok_or("printf not declared")?;

        let inferred = self.infer_type(expr);

        let format_str = match value {
            BasicValueEnum::IntValue(_) => "%lld\n",
            BasicValueEnum::FloatValue(_) => "%f\n",
            BasicValueEnum::PointerValue(_) => {
                if inferred == Type::Text {
                    "%s\n"
                } else {
                    "%p\n"
                }
            }
            _ => return Err("Cannot print this type".to_string()),
        };

        let format_ptr = self
            .builder
            .build_global_string_ptr(format_str, "fmt")
            .map_err(|e| e.to_string())?;

        self.builder
            .build_call(
                printf,
                &[format_ptr.as_pointer_value().into(), value.into()],
                "printf_call",
            )
            .map_err(|e| e.to_string())?;

        Ok(())
    }

    fn get_llvm_type(&self, typ: &Type) -> inkwell::types::BasicTypeEnum<'ctx> {
        match typ {
            Type::Int => self.context.i64_type().into(),
            Type::Float => self.context.f64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Text => self.context.ptr_type(inkwell::AddressSpace::default()).into(),
            Type::Class(_) => self.context.ptr_type(inkwell::AddressSpace::default()).into(),
            Type::Void => self.context.i64_type().into(), // shouldn't be used as value type
            _ => self.context.i64_type().into(),
        }
    }

    fn infer_type(&self, expr: &Expr) -> Type {
        match expr {
            Expr::IntLiteral(_) => Type::Int,
            Expr::FloatLiteral(_) => Type::Float,
            Expr::StringLiteral(_) => Type::Text,
            Expr::BoolLiteral(_) => Type::Bool,
            Expr::Identifier(name) => self
                .variables
                .get(name)
                .map(|(_, t)| t.clone())
                .unwrap_or(Type::Int),
            Expr::BinaryOp { left, right, .. } => {
                let left_type = self.infer_type(left);
                let right_type = self.infer_type(right);
                if left_type == Type::Float || right_type == Type::Float {
                    Type::Float
                } else {
                    Type::Int
                }
            }
            Expr::FunctionCall { name, .. } => self
                .functions
                .get(name)
                .and_then(|f| {
                    let ft = f.get_type();
                    ft.get_return_type().map(|_| {
                        // We don't have direct ast-level info here, so approximate
                        Type::Int
                    })
                })
                .unwrap_or(Type::Int),
            Expr::NewObject { class_name, .. } => Type::Class(class_name.clone()),
            Expr::MethodCall { object, .. } => {
                // Approximate: infer object's class then look up method return type
                self.infer_type(object)
            }
            Expr::PropertyAccess { object, property } => {
                if let Type::Class(class_name) = self.infer_type(object) {
                    self.class_field_types
                        .get(&class_name)
                        .and_then(|m| m.get(property))
                        .cloned()
                        .unwrap_or(Type::Int)
                } else {
                    Type::Int
                }
            }
            Expr::TypeConversion { target_type, .. } => target_type.clone(),
            _ => Type::Int,
        }
    }

    pub fn write_object_file(&self, path: &Path) -> Result<(), String> {
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| format!("Failed to initialize native target: {}", e))?;

        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple)
            .map_err(|e| format!("Failed to get target: {}", e))?;

        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or("Failed to create target machine")?;

        target_machine
            .write_to_file(&self.module, FileType::Object, path)
            .map_err(|e| format!("Failed to write object file: {}", e))
    }

    pub fn print_ir(&self) {
        println!("{}", self.module.print_to_string().to_string());
    }
}
