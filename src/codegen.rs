use crate::ast::*;
use crate::stdlib;
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
    /// Builtin standard library function values
    builtin_functions: HashMap<String, FunctionValue<'ctx>>,
    /// Return type info for builtins (needed for infer_type)
    builtin_return_types: HashMap<String, Type>,
    /// Builtin metadata for auto-promotion checks
    builtin_metadata: HashMap<String, stdlib::BuiltinFunction>,
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
            builtin_functions: HashMap::new(),
            builtin_return_types: HashMap::new(),
            builtin_metadata: HashMap::new(),
        }
    }

    pub fn compile(&mut self, program: &Program) -> Result<(), String> {
        self.declare_printf();
        self.declare_malloc();
        self.declare_builtin_functions()?;

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

    // ========== Builtin Standard Library ==========

    fn declare_builtin_functions(&mut self) -> Result<(), String> {
        let f64_type = self.context.f64_type();
        let i64_type = self.context.i64_type();
        let i32_type = self.context.i32_type();
        let i8_type = self.context.i8_type();
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let bool_type = self.context.bool_type();

        // Declare C math externals and register under all alias names
        let mut declared_c_fns: HashMap<String, FunctionValue<'ctx>> = HashMap::new();

        for builtin in stdlib::get_math_builtins() {
            let c_name = builtin.c_function.unwrap();

            let fn_value = if let Some(existing) = declared_c_fns.get(c_name) {
                *existing
            } else {
                let param_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
                    builtin.parameters.iter().map(|_| f64_type.into()).collect();
                let fn_type = f64_type.fn_type(&param_types, false);
                let fv = self.module.add_function(c_name, fn_type, None);
                declared_c_fns.insert(c_name.to_string(), fv);
                fv
            };

            for name in &builtin.names {
                self.builtin_functions.insert(name.to_string(), fn_value);
                self.builtin_return_types
                    .insert(name.to_string(), builtin.return_type.clone());
                self.builtin_metadata
                    .insert(name.to_string(), builtin.clone());
            }
        }

        // Declare C string helpers (internal, not user-callable)
        let strlen_type = i64_type.fn_type(&[ptr_type.into()], false);
        let c_strlen = self.module.add_function("strlen", strlen_type, None);

        let strcpy_type = ptr_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
        let c_strcpy = self.module.add_function("strcpy", strcpy_type, None);

        let strcat_type = ptr_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
        let c_strcat = self.module.add_function("strcat", strcat_type, None);

        let strstr_type = ptr_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
        let c_strstr = self.module.add_function("strstr", strstr_type, None);

        let toupper_type = i32_type.fn_type(&[i32_type.into()], false);
        let c_toupper = self.module.add_function("toupper", toupper_type, None);

        let tolower_type = i32_type.fn_type(&[i32_type.into()], false);
        let c_tolower = self.module.add_function("tolower", tolower_type, None);

        let malloc = self.malloc_fn.ok_or("malloc not declared")?;

        // Build custom string functions

        // --- lengthOf(s: ptr) -> i64 ---
        {
            let fn_type = i64_type.fn_type(&[ptr_type.into()], false);
            let func = self.module.add_function("englang_strlen", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let s = func.get_nth_param(0).unwrap().into_pointer_value();
            let len = self
                .builder
                .build_call(c_strlen, &[s.into()], "len")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let len_val = match len {
                ValueKind::Basic(v) => v,
                _ => return Err("strlen returned void".to_string()),
            };
            self.builder
                .build_return(Some(&len_val))
                .map_err(|e| e.to_string())?;

            for name in &["lengthOf", "strlen"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types
                    .insert(name.to_string(), Type::Int);
            }
        }

        // --- combine(a: ptr, b: ptr) -> ptr ---
        {
            let fn_type = ptr_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
            let func = self.module.add_function("englang_combine", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let a = func.get_nth_param(0).unwrap().into_pointer_value();
            let b = func.get_nth_param(1).unwrap().into_pointer_value();

            let len_a = self
                .builder
                .build_call(c_strlen, &[a.into()], "len_a")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let len_a = match len_a {
                ValueKind::Basic(v) => v.into_int_value(),
                _ => return Err("strlen returned void".to_string()),
            };

            let len_b = self
                .builder
                .build_call(c_strlen, &[b.into()], "len_b")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let len_b = match len_b {
                ValueKind::Basic(v) => v.into_int_value(),
                _ => return Err("strlen returned void".to_string()),
            };

            let total = self
                .builder
                .build_int_add(len_a, len_b, "total")
                .map_err(|e| e.to_string())?;
            let total_plus1 = self
                .builder
                .build_int_add(total, i64_type.const_int(1, false), "total_plus1")
                .map_err(|e| e.to_string())?;

            let buf = self
                .builder
                .build_call(malloc, &[total_plus1.into()], "buf")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let buf = match buf {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("malloc returned void".to_string()),
            };

            self.builder
                .build_call(c_strcpy, &[buf.into(), a.into()], "cpy")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_call(c_strcat, &[buf.into(), b.into()], "cat")
                .map_err(|e| e.to_string())?;

            self.builder
                .build_return(Some(&buf))
                .map_err(|e| e.to_string())?;

            for name in &["combine", "concat"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types
                    .insert(name.to_string(), Type::Text);
            }
        }

        // --- characterAt(s: ptr, idx: i64) -> ptr ---
        {
            let fn_type = ptr_type.fn_type(&[ptr_type.into(), i64_type.into()], false);
            let func = self.module.add_function("englang_charAt", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let s = func.get_nth_param(0).unwrap().into_pointer_value();
            let idx = func.get_nth_param(1).unwrap().into_int_value();

            let buf = self
                .builder
                .build_call(malloc, &[i64_type.const_int(2, false).into()], "buf")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let buf = match buf {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("malloc returned void".to_string()),
            };

            let char_ptr = unsafe {
                self.builder
                    .build_gep(i8_type, s, &[idx], "char_ptr")
                    .map_err(|e| e.to_string())?
            };
            let ch = self
                .builder
                .build_load(i8_type, char_ptr, "ch")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(buf, ch)
                .map_err(|e| e.to_string())?;

            let null_ptr = unsafe {
                self.builder
                    .build_gep(
                        i8_type,
                        buf,
                        &[i64_type.const_int(1, false)],
                        "null_ptr",
                    )
                    .map_err(|e| e.to_string())?
            };
            self.builder
                .build_store(null_ptr, i8_type.const_int(0, false))
                .map_err(|e| e.to_string())?;

            self.builder
                .build_return(Some(&buf))
                .map_err(|e| e.to_string())?;

            for name in &["characterAt", "charAt"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types
                    .insert(name.to_string(), Type::Text);
            }
        }

        // --- uppercase(s: ptr) -> ptr ---
        {
            let func = self.build_case_function("englang_uppercase", c_toupper)?;
            for name in &["uppercase", "upper"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types
                    .insert(name.to_string(), Type::Text);
            }
        }

        // --- lowercase(s: ptr) -> ptr ---
        {
            let func = self.build_case_function("englang_lowercase", c_tolower)?;
            for name in &["lowercase", "lower"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types
                    .insert(name.to_string(), Type::Text);
            }
        }

        // --- contains(haystack: ptr, needle: ptr) -> i1 ---
        {
            let fn_type = bool_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
            let func = self
                .module
                .add_function("englang_contains", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let haystack = func.get_nth_param(0).unwrap().into_pointer_value();
            let needle = func.get_nth_param(1).unwrap().into_pointer_value();

            let result = self
                .builder
                .build_call(c_strstr, &[haystack.into(), needle.into()], "found")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let result_ptr = match result {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("strstr returned void".to_string()),
            };

            let null_ptr = ptr_type.const_null();
            let is_found = self
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::NE,
                    result_ptr,
                    null_ptr,
                    "is_found",
                )
                .map_err(|e| e.to_string())?;

            self.builder
                .build_return(Some(&is_found))
                .map_err(|e| e.to_string())?;

            for name in &["contains", "has"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types
                    .insert(name.to_string(), Type::Bool);
            }
        }

        // Register metadata for string builtins
        for builtin in stdlib::get_string_builtins() {
            for name in &builtin.names {
                self.builtin_metadata
                    .insert(name.to_string(), builtin.clone());
            }
        }

        // --- englang_print_float(val: f64) -> void ---
        // Formats with %.10g, ensures at least one decimal place (e.g. 4 -> "4.0")
        {
            let printf = self.printf_fn.ok_or("printf not declared")?;

            let snprintf_type =
                i32_type.fn_type(&[ptr_type.into(), i64_type.into(), ptr_type.into()], true);
            let c_snprintf = self.module.add_function("snprintf", snprintf_type, None);

            let strchr_type = ptr_type.fn_type(&[ptr_type.into(), i32_type.into()], false);
            let c_strchr = self.module.add_function("strchr", strchr_type, None);

            let void_type = self.context.void_type();
            let fn_type = void_type.fn_type(&[f64_type.into()], false);
            let func = self
                .module
                .add_function("englang_print_float", fn_type, None);

            let entry = self.context.append_basic_block(func, "entry");
            let has_dot_bb = self.context.append_basic_block(func, "has_dot");
            let no_dot_bb = self.context.append_basic_block(func, "no_dot");

            // entry:
            self.builder.position_at_end(entry);
            let val = func.get_nth_param(0).unwrap().into_float_value();

            let buf_array_type = i8_type.array_type(64);
            let buf = self
                .builder
                .build_alloca(buf_array_type, "buf")
                .map_err(|e| e.to_string())?;

            let gfmt = self
                .builder
                .build_global_string_ptr("%.10g", "gfmt")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_call(
                    c_snprintf,
                    &[
                        buf.into(),
                        i64_type.const_int(64, false).into(),
                        gfmt.as_pointer_value().into(),
                        val.into(),
                    ],
                    "snp",
                )
                .map_err(|e| e.to_string())?;

            // strchr(buf, '.')
            let dot_result = self
                .builder
                .build_call(
                    c_strchr,
                    &[buf.into(), i32_type.const_int(46, false).into()],
                    "dot",
                )
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let dot_ptr = match dot_result {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("strchr returned void".to_string()),
            };

            let is_null = self
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::EQ,
                    dot_ptr,
                    ptr_type.const_null(),
                    "is_null",
                )
                .map_err(|e| e.to_string())?;
            self.builder
                .build_conditional_branch(is_null, no_dot_bb, has_dot_bb)
                .map_err(|e| e.to_string())?;

            // no_dot: printf("%s.0\n", buf)
            self.builder.position_at_end(no_dot_bb);
            let fmt_no_dot = self
                .builder
                .build_global_string_ptr("%s.0\n", "fmt_nodot")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_call(
                    printf,
                    &[fmt_no_dot.as_pointer_value().into(), buf.into()],
                    "p_nodot",
                )
                .map_err(|e| e.to_string())?;
            self.builder
                .build_return(None)
                .map_err(|e| e.to_string())?;

            // has_dot: printf("%s\n", buf)
            self.builder.position_at_end(has_dot_bb);
            let fmt_has_dot = self
                .builder
                .build_global_string_ptr("%s\n", "fmt_hasdot")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_call(
                    printf,
                    &[fmt_has_dot.as_pointer_value().into(), buf.into()],
                    "p_hasdot",
                )
                .map_err(|e| e.to_string())?;
            self.builder
                .build_return(None)
                .map_err(|e| e.to_string())?;
        }

        Ok(())
    }

    /// Build an uppercase or lowercase function (shared logic, different C helper)
    fn build_case_function(
        &mut self,
        name: &str,
        case_fn: FunctionValue<'ctx>,
    ) -> Result<FunctionValue<'ctx>, String> {
        let i64_type = self.context.i64_type();
        let i32_type = self.context.i32_type();
        let i8_type = self.context.i8_type();
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());

        let c_strlen = self
            .module
            .get_function("strlen")
            .ok_or("strlen not declared")?;
        let malloc = self.malloc_fn.ok_or("malloc not declared")?;

        let fn_type = ptr_type.fn_type(&[ptr_type.into()], false);
        let func = self.module.add_function(name, fn_type, None);

        let entry = self.context.append_basic_block(func, "entry");
        let loop_cond = self.context.append_basic_block(func, "loop_cond");
        let loop_body = self.context.append_basic_block(func, "loop_body");
        let loop_end = self.context.append_basic_block(func, "loop_end");

        // entry:
        self.builder.position_at_end(entry);
        let s = func.get_nth_param(0).unwrap().into_pointer_value();

        let len = self
            .builder
            .build_call(c_strlen, &[s.into()], "len")
            .map_err(|e| e.to_string())?
            .try_as_basic_value();
        let len = match len {
            ValueKind::Basic(v) => v.into_int_value(),
            _ => return Err("strlen returned void".to_string()),
        };

        let len_plus1 = self
            .builder
            .build_int_add(len, i64_type.const_int(1, false), "len_plus1")
            .map_err(|e| e.to_string())?;
        let buf = self
            .builder
            .build_call(malloc, &[len_plus1.into()], "buf")
            .map_err(|e| e.to_string())?
            .try_as_basic_value();
        let buf = match buf {
            ValueKind::Basic(v) => v.into_pointer_value(),
            _ => return Err("malloc returned void".to_string()),
        };

        self.builder
            .build_unconditional_branch(loop_cond)
            .map_err(|e| e.to_string())?;

        // loop_cond:
        self.builder.position_at_end(loop_cond);
        let i_phi = self
            .builder
            .build_phi(i64_type, "i")
            .map_err(|e| e.to_string())?;
        i_phi.add_incoming(&[(&i64_type.const_int(0, false), entry)]);
        let i_val = i_phi.as_basic_value().into_int_value();

        let cmp = self
            .builder
            .build_int_compare(inkwell::IntPredicate::SLT, i_val, len, "cmp")
            .map_err(|e| e.to_string())?;
        self.builder
            .build_conditional_branch(cmp, loop_body, loop_end)
            .map_err(|e| e.to_string())?;

        // loop_body:
        self.builder.position_at_end(loop_body);
        let src_ptr = unsafe {
            self.builder
                .build_gep(i8_type, s, &[i_val], "src_ptr")
                .map_err(|e| e.to_string())?
        };
        let ch = self
            .builder
            .build_load(i8_type, src_ptr, "ch")
            .map_err(|e| e.to_string())?
            .into_int_value();

        // Sign-extend i8 to i32 for toupper/tolower
        let ch_i32 = self
            .builder
            .build_int_s_extend(ch, i32_type, "ch_i32")
            .map_err(|e| e.to_string())?;
        let converted = self
            .builder
            .build_call(case_fn, &[ch_i32.into()], "converted")
            .map_err(|e| e.to_string())?
            .try_as_basic_value();
        let converted_i32 = match converted {
            ValueKind::Basic(v) => v.into_int_value(),
            _ => return Err("toupper/tolower returned void".to_string()),
        };
        let converted_i8 = self
            .builder
            .build_int_truncate(converted_i32, i8_type, "conv_i8")
            .map_err(|e| e.to_string())?;

        let dst_ptr = unsafe {
            self.builder
                .build_gep(i8_type, buf, &[i_val], "dst_ptr")
                .map_err(|e| e.to_string())?
        };
        self.builder
            .build_store(dst_ptr, converted_i8)
            .map_err(|e| e.to_string())?;

        let i_next = self
            .builder
            .build_int_add(i_val, i64_type.const_int(1, false), "i_next")
            .map_err(|e| e.to_string())?;
        i_phi.add_incoming(&[(&i_next, loop_body)]);
        self.builder
            .build_unconditional_branch(loop_cond)
            .map_err(|e| e.to_string())?;

        // loop_end: null-terminate and return
        self.builder.position_at_end(loop_end);
        let null_ptr = unsafe {
            self.builder
                .build_gep(i8_type, buf, &[len], "null_pos")
                .map_err(|e| e.to_string())?
        };
        self.builder
            .build_store(null_ptr, i8_type.const_int(0, false))
            .map_err(|e| e.to_string())?;
        self.builder
            .build_return(Some(&buf))
            .map_err(|e| e.to_string())?;

        Ok(func)
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
                // Check builtins first, then user-defined functions
                let fn_value = if let Some(fv) = self.builtin_functions.get(name) {
                    *fv
                } else {
                    *self
                        .functions
                        .get(name)
                        .ok_or_else(|| format!("Undefined function: {}", name))?
                };

                let builtin_meta = self.builtin_metadata.get(name).cloned();

                let mut args: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> = Vec::new();
                for (i, arg) in arguments.iter().enumerate() {
                    let compiled_arg = self.compile_expression(arg)?;

                    // Auto-promote Int to Float for builtin math functions
                    if let Some(ref meta) = builtin_meta {
                        if meta.accepts_int_as_float {
                            if let Some((_, Type::Float)) = meta.parameters.get(i) {
                                if let BasicValueEnum::IntValue(iv) = compiled_arg {
                                    let promoted = self
                                        .builder
                                        .build_signed_int_to_float(
                                            iv,
                                            self.context.f64_type(),
                                            "promote",
                                        )
                                        .map_err(|e| e.to_string())?;
                                    args.push(promoted.into());
                                    continue;
                                }
                            }
                        }
                    }

                    args.push(compiled_arg.into());
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

        // Floats: use custom formatter that ensures at least one decimal place
        if let BasicValueEnum::FloatValue(fv) = value {
            let print_float = self
                .module
                .get_function("englang_print_float")
                .ok_or("englang_print_float not declared")?;
            self.builder
                .build_call(print_float, &[fv.into()], "pf")
                .map_err(|e| e.to_string())?;
            return Ok(());
        }

        let inferred = self.infer_type(expr);

        let format_str = match value {
            BasicValueEnum::IntValue(_) => "%lld\n",
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
            Expr::FunctionCall { name, .. } => {
                // Check builtin return types first
                if let Some(return_type) = self.builtin_return_types.get(name) {
                    return return_type.clone();
                }
                self.functions
                    .get(name)
                    .and_then(|f| {
                        let ft = f.get_type();
                        ft.get_return_type().map(|_| Type::Int)
                    })
                    .unwrap_or(Type::Int)
            }
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
