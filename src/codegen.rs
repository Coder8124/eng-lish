use crate::ast::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::path::Path;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, (PointerValue<'ctx>, Type)>,
    printf_fn: Option<FunctionValue<'ctx>>,
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
        }
    }

    pub fn compile(&mut self, program: &Program) -> Result<(), String> {
        self.declare_printf();
        self.create_main_function(program)?;
        Ok(())
    }

    fn declare_printf(&mut self) {
        let i32_type = self.context.i32_type();
        let i8_ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let printf_type = i32_type.fn_type(&[i8_ptr_type.into()], true);
        self.printf_fn = Some(self.module.add_function("printf", printf_type, None));
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

        let format_str = match value {
            BasicValueEnum::IntValue(_) => "%lld\n",
            BasicValueEnum::FloatValue(_) => "%f\n",
            BasicValueEnum::PointerValue(_) => {
                if matches!(expr, Expr::StringLiteral(_)) {
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
