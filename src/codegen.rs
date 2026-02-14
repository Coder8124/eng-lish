use crate::ast::*;
use crate::stdlib;
use inkwell::basic_block::BasicBlock;
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
    /// Stack of break targets (for nested loops)
    loop_break_stack: Vec<BasicBlock<'ctx>>,
    /// Stack of continue targets (for nested loops)
    loop_continue_stack: Vec<BasicBlock<'ctx>>,
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
            loop_break_stack: Vec::new(),
            loop_continue_stack: Vec::new(),
        }
    }

    /// Check if the current basic block already has a terminator (branch, return, etc.)
    fn current_block_has_terminator(&self) -> bool {
        if let Some(block) = self.builder.get_insert_block() {
            block.get_terminator().is_some()
        } else {
            false
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

        // Declare additional C functions for I/O and utilities
        let scanf_type = i32_type.fn_type(&[ptr_type.into()], true); // varargs
        let c_scanf = self.module.add_function("scanf", scanf_type, None);

        // Get or declare snprintf (may already exist from float printing)
        let c_snprintf = self.module.get_function("snprintf").unwrap_or_else(|| {
            let snprintf_type =
                i32_type.fn_type(&[ptr_type.into(), i64_type.into(), ptr_type.into()], true);
            self.module.add_function("snprintf", snprintf_type, None)
        });

        let usleep_type = i32_type.fn_type(&[i32_type.into()], false);
        let c_usleep = self.module.add_function("usleep", usleep_type, None);

        let atoll_type = i64_type.fn_type(&[ptr_type.into()], false);
        let c_atoll = self.module.add_function("atoll", atoll_type, None);

        let atof_type = f64_type.fn_type(&[ptr_type.into()], false);
        let c_atof = self.module.add_function("atof", atof_type, None);

        let getchar_type = i32_type.fn_type(&[], false);
        let c_getchar = self.module.add_function("getchar", getchar_type, None);

        // C functions for random number generation
        let rand_type = i32_type.fn_type(&[], false);
        let c_rand = self.module.add_function("rand", rand_type, None);

        let srand_type = self.context.void_type().fn_type(&[i32_type.into()], false);
        let c_srand = self.module.add_function("srand", srand_type, None);

        let time_type = i64_type.fn_type(&[ptr_type.into()], false);
        let c_time = self.module.add_function("time", time_type, None);

        // C functions for file I/O
        let fopen_type = ptr_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
        let c_fopen = self.module.add_function("fopen", fopen_type, None);

        let fclose_type = i32_type.fn_type(&[ptr_type.into()], false);
        let c_fclose = self.module.add_function("fclose", fclose_type, None);

        // fprintf is variadic, but we'll use it with known patterns
        let fprintf_type = i32_type.fn_type(&[ptr_type.into(), ptr_type.into()], true);
        let _c_fprintf = self.module.add_function("fprintf", fprintf_type, None);

        let fseek_type = i32_type.fn_type(&[ptr_type.into(), i64_type.into(), i32_type.into()], false);
        let c_fseek = self.module.add_function("fseek", fseek_type, None);

        let ftell_type = i64_type.fn_type(&[ptr_type.into()], false);
        let c_ftell = self.module.add_function("ftell", ftell_type, None);

        let rewind_type = self.context.void_type().fn_type(&[ptr_type.into()], false);
        let c_rewind = self.module.add_function("rewind", rewind_type, None);

        let fread_type = i64_type.fn_type(&[ptr_type.into(), i64_type.into(), i64_type.into(), ptr_type.into()], false);
        let c_fread = self.module.add_function("fread", fread_type, None);

        let fwrite_type = i64_type.fn_type(&[ptr_type.into(), i64_type.into(), i64_type.into(), ptr_type.into()], false);
        let c_fwrite = self.module.add_function("fwrite", fwrite_type, None);

        // --- readLine() -> ptr ---
        // Uses scanf with %[^\n] to read until newline, then getchar to consume the newline
        {
            let fn_type = ptr_type.fn_type(&[], false);
            let func = self.module.add_function("englang_readLine", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            // Allocate a buffer of 1024 bytes
            let buf_size = i64_type.const_int(1024, false);
            let buf_result = self
                .builder
                .build_call(malloc, &[buf_size.into()], "buf")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let buf = match buf_result {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("malloc returned void".to_string()),
            };

            // Initialize buffer with empty string
            let null_byte = self.context.i8_type().const_int(0, false);
            self.builder
                .build_store(buf, null_byte)
                .map_err(|e| e.to_string())?;

            // scanf("%1023[^\n]", buf) - read up to 1023 chars until newline
            let fmt = self
                .builder
                .build_global_string_ptr("%1023[^\n]", "readline_fmt")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_call(
                    c_scanf,
                    &[fmt.as_pointer_value().into(), buf.into()],
                    "scanf_result",
                )
                .map_err(|e| e.to_string())?;

            // Consume the trailing newline with getchar()
            self.builder
                .build_call(c_getchar, &[], "getchar_result")
                .map_err(|e| e.to_string())?;

            self.builder
                .build_return(Some(&buf))
                .map_err(|e| e.to_string())?;

            for name in &["readLine", "readln", "input"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types
                    .insert(name.to_string(), Type::Text);
            }
        }

        // --- readNumber() -> i64 ---
        {
            let fn_type = i64_type.fn_type(&[], false);
            let func = self.module.add_function("englang_readNumber", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            // Allocate space for the number
            let num_ptr = self
                .builder
                .build_alloca(i64_type, "num")
                .map_err(|e| e.to_string())?;

            // Create format string for scanf
            let fmt = self
                .builder
                .build_global_string_ptr("%lld", "scanf_fmt")
                .map_err(|e| e.to_string())?;

            // Call scanf
            self.builder
                .build_call(
                    c_scanf,
                    &[fmt.as_pointer_value().into(), num_ptr.into()],
                    "scanf_result",
                )
                .map_err(|e| e.to_string())?;

            // Load and return the number
            let num = self
                .builder
                .build_load(i64_type, num_ptr, "num_val")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_return(Some(&num))
                .map_err(|e| e.to_string())?;

            for name in &["readNumber", "readnum"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types
                    .insert(name.to_string(), Type::Int);
            }
        }

        // --- textToNumber(s: ptr) -> i64 ---
        {
            let fn_type = i64_type.fn_type(&[ptr_type.into()], false);
            let func = self.module.add_function("englang_textToNumber", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let s = func.get_nth_param(0).unwrap().into_pointer_value();
            let result_val = self
                .builder
                .build_call(c_atoll, &[s.into()], "atoll_result")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let result = match result_val {
                ValueKind::Basic(v) => v,
                _ => return Err("atoll returned void".to_string()),
            };
            self.builder
                .build_return(Some(&result))
                .map_err(|e| e.to_string())?;

            for name in &["textToNumber", "parseInt"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types
                    .insert(name.to_string(), Type::Int);
            }
        }

        // --- textToDecimal(s: ptr) -> f64 ---
        {
            let fn_type = f64_type.fn_type(&[ptr_type.into()], false);
            let func = self.module.add_function("englang_textToDecimal", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let s = func.get_nth_param(0).unwrap().into_pointer_value();
            let result_val = self
                .builder
                .build_call(c_atof, &[s.into()], "atof_result")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let result = match result_val {
                ValueKind::Basic(v) => v,
                _ => return Err("atof returned void".to_string()),
            };
            self.builder
                .build_return(Some(&result))
                .map_err(|e| e.to_string())?;

            for name in &["textToDecimal", "parseFloat"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types
                    .insert(name.to_string(), Type::Float);
            }
        }

        // --- numberToText(n: i64) -> ptr ---
        {
            let fn_type = ptr_type.fn_type(&[i64_type.into()], false);
            let func = self.module.add_function("englang_numberToText", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let n = func.get_nth_param(0).unwrap().into_int_value();

            // Allocate buffer (32 bytes is enough for any 64-bit integer)
            let buf_size = i64_type.const_int(32, false);
            let buf_result = self
                .builder
                .build_call(malloc, &[buf_size.into()], "buf")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let buf = match buf_result {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("malloc returned void".to_string()),
            };

            // snprintf(buf, 32, "%lld", n)
            let fmt = self
                .builder
                .build_global_string_ptr("%lld", "int_fmt")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_call(
                    c_snprintf,
                    &[buf.into(), buf_size.into(), fmt.as_pointer_value().into(), n.into()],
                    "snprintf_result",
                )
                .map_err(|e| e.to_string())?;

            self.builder
                .build_return(Some(&buf))
                .map_err(|e| e.to_string())?;

            for name in &["numberToText", "intToString"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types
                    .insert(name.to_string(), Type::Text);
            }
        }

        // --- decimalToText(f: f64) -> ptr ---
        {
            let fn_type = ptr_type.fn_type(&[f64_type.into()], false);
            let func = self.module.add_function("englang_decimalToText", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let f = func.get_nth_param(0).unwrap().into_float_value();

            // Allocate buffer (64 bytes should be enough for most floats)
            let buf_size = i64_type.const_int(64, false);
            let buf_result = self
                .builder
                .build_call(malloc, &[buf_size.into()], "buf")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let buf = match buf_result {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("malloc returned void".to_string()),
            };

            // snprintf(buf, 64, "%.10g", f)
            let fmt = self
                .builder
                .build_global_string_ptr("%.10g", "float_fmt")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_call(
                    c_snprintf,
                    &[buf.into(), buf_size.into(), fmt.as_pointer_value().into(), f.into()],
                    "snprintf_result",
                )
                .map_err(|e| e.to_string())?;

            self.builder
                .build_return(Some(&buf))
                .map_err(|e| e.to_string())?;

            for name in &["decimalToText", "floatToString"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types
                    .insert(name.to_string(), Type::Text);
            }
        }

        // --- sleep(ms: i64) -> void ---
        {
            let void_type = self.context.void_type();
            let fn_type = void_type.fn_type(&[i64_type.into()], false);
            let func = self.module.add_function("englang_sleep", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let ms = func.get_nth_param(0).unwrap().into_int_value();

            // Convert ms to microseconds (ms * 1000)
            let thousand = i64_type.const_int(1000, false);
            let us = self
                .builder
                .build_int_mul(ms, thousand, "us")
                .map_err(|e| e.to_string())?;

            // usleep takes u32, so truncate
            let us_32 = self
                .builder
                .build_int_truncate(us, i32_type, "us_32")
                .map_err(|e| e.to_string())?;

            self.builder
                .build_call(c_usleep, &[us_32.into()], "usleep_result")
                .map_err(|e| e.to_string())?;

            self.builder
                .build_return(None)
                .map_err(|e| e.to_string())?;

            for name in &["sleep", "wait", "delay"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types
                    .insert(name.to_string(), Type::Void);
            }
        }

        // --- random() -> f64 ---
        // Returns a random float between 0.0 and 1.0
        {
            let fn_type = f64_type.fn_type(&[], false);
            let func = self.module.add_function("englang_random", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            // Seed the random number generator on first call using a global flag
            let seeded_global = self.module.add_global(i32_type, None, "random_seeded");
            seeded_global.set_initializer(&i32_type.const_zero());

            let seeded_val = self
                .builder
                .build_load(i32_type, seeded_global.as_pointer_value(), "seeded")
                .map_err(|e| e.to_string())?
                .into_int_value();

            let is_seeded = self
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::NE,
                    seeded_val,
                    i32_type.const_zero(),
                    "is_seeded",
                )
                .map_err(|e| e.to_string())?;

            let seed_bb = self.context.append_basic_block(func, "seed");
            let continue_bb = self.context.append_basic_block(func, "continue");

            self.builder
                .build_conditional_branch(is_seeded, continue_bb, seed_bb)
                .map_err(|e| e.to_string())?;

            // Seed block
            self.builder.position_at_end(seed_bb);
            let null_ptr = ptr_type.const_null();
            let time_val = self
                .builder
                .build_call(c_time, &[null_ptr.into()], "time")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let time_int = match time_val {
                ValueKind::Basic(v) => v.into_int_value(),
                _ => return Err("time returned void".to_string()),
            };
            let time_32 = self
                .builder
                .build_int_truncate(time_int, i32_type, "time_32")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_call(c_srand, &[time_32.into()], "")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_store(seeded_global.as_pointer_value(), i32_type.const_int(1, false))
                .map_err(|e| e.to_string())?;
            self.builder
                .build_unconditional_branch(continue_bb)
                .map_err(|e| e.to_string())?;

            // Continue block - generate random number
            self.builder.position_at_end(continue_bb);
            let rand_val = self
                .builder
                .build_call(c_rand, &[], "rand")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let rand_int = match rand_val {
                ValueKind::Basic(v) => v.into_int_value(),
                _ => return Err("rand returned void".to_string()),
            };

            // Convert to float and divide by RAND_MAX (2147483647)
            let rand_float = self
                .builder
                .build_signed_int_to_float(rand_int, f64_type, "rand_float")
                .map_err(|e| e.to_string())?;
            let rand_max = f64_type.const_float(2147483647.0);
            let result = self
                .builder
                .build_float_div(rand_float, rand_max, "result")
                .map_err(|e| e.to_string())?;

            self.builder
                .build_return(Some(&result))
                .map_err(|e| e.to_string())?;

            self.builtin_functions.insert("random".to_string(), func);
            self.builtin_return_types
                .insert("random".to_string(), Type::Float);
        }

        // --- randomBetween(min: i64, max: i64) -> i64 ---
        // Returns a random integer between min and max (inclusive)
        {
            let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into()], false);
            let func = self.module.add_function("englang_randomBetween", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let min = func.get_nth_param(0).unwrap().into_int_value();
            let max = func.get_nth_param(1).unwrap().into_int_value();

            // Call rand()
            let rand_val = self
                .builder
                .build_call(c_rand, &[], "rand")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let rand_int = match rand_val {
                ValueKind::Basic(v) => v.into_int_value(),
                _ => return Err("rand returned void".to_string()),
            };

            // Extend rand result to i64
            let rand_64 = self
                .builder
                .build_int_z_extend(rand_int, i64_type, "rand_64")
                .map_err(|e| e.to_string())?;

            // range = max - min + 1
            let range = self
                .builder
                .build_int_sub(max, min, "range")
                .map_err(|e| e.to_string())?;
            let range_plus_1 = self
                .builder
                .build_int_add(range, i64_type.const_int(1, false), "range_plus_1")
                .map_err(|e| e.to_string())?;

            // result = min + (rand % range_plus_1)
            let modulo = self
                .builder
                .build_int_signed_rem(rand_64, range_plus_1, "modulo")
                .map_err(|e| e.to_string())?;
            let result = self
                .builder
                .build_int_add(min, modulo, "result")
                .map_err(|e| e.to_string())?;

            self.builder
                .build_return(Some(&result))
                .map_err(|e| e.to_string())?;

            for name in &["randomBetween", "randomInt"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types
                    .insert(name.to_string(), Type::Int);
            }
        }

        // --- readFile(path: ptr) -> ptr ---
        // Reads entire file contents as a string
        {
            let fn_type = ptr_type.fn_type(&[ptr_type.into()], false);
            let func = self.module.add_function("englang_readFile", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let path = func.get_nth_param(0).unwrap().into_pointer_value();

            // Create "r" mode string
            let mode_str = self
                .builder
                .build_global_string_ptr("r", "read_mode")
                .map_err(|e| e.to_string())?;

            // Open file
            let file = self
                .builder
                .build_call(c_fopen, &[path.into(), mode_str.as_pointer_value().into()], "file")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let file_ptr = match file {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("fopen returned void".to_string()),
            };

            // Check if file is null
            let is_null = self
                .builder
                .build_is_null(file_ptr, "is_null")
                .map_err(|e| e.to_string())?;

            let null_bb = self.context.append_basic_block(func, "null_file");
            let valid_bb = self.context.append_basic_block(func, "valid_file");

            self.builder
                .build_conditional_branch(is_null, null_bb, valid_bb)
                .map_err(|e| e.to_string())?;

            // Null file - return empty string
            self.builder.position_at_end(null_bb);
            let empty_str = self
                .builder
                .build_global_string_ptr("", "empty")
                .map_err(|e| e.to_string())?;
            self.builder
                .build_return(Some(&empty_str.as_pointer_value()))
                .map_err(|e| e.to_string())?;

            // Valid file - read contents
            self.builder.position_at_end(valid_bb);

            // Seek to end to get file size
            let seek_end = i32_type.const_int(2, false); // SEEK_END
            self.builder
                .build_call(c_fseek, &[file_ptr.into(), i64_type.const_zero().into(), seek_end.into()], "")
                .map_err(|e| e.to_string())?;

            // Get file size
            let size = self
                .builder
                .build_call(c_ftell, &[file_ptr.into()], "size")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let file_size = match size {
                ValueKind::Basic(v) => v.into_int_value(),
                _ => return Err("ftell returned void".to_string()),
            };

            // Rewind to beginning
            self.builder
                .build_call(c_rewind, &[file_ptr.into()], "")
                .map_err(|e| e.to_string())?;

            // Allocate buffer (size + 1 for null terminator)
            let buf_size = self
                .builder
                .build_int_add(file_size, i64_type.const_int(1, false), "buf_size")
                .map_err(|e| e.to_string())?;
            let buf = self
                .builder
                .build_call(malloc, &[buf_size.into()], "buf")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let buf_ptr = match buf {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("malloc returned void".to_string()),
            };

            // Read file contents
            self.builder
                .build_call(c_fread, &[buf_ptr.into(), i64_type.const_int(1, false).into(), file_size.into(), file_ptr.into()], "")
                .map_err(|e| e.to_string())?;

            // Null terminate the buffer
            let null_pos = unsafe {
                self.builder
                    .build_gep(i8_type, buf_ptr, &[file_size], "null_pos")
                    .map_err(|e| e.to_string())?
            };
            self.builder
                .build_store(null_pos, i8_type.const_zero())
                .map_err(|e| e.to_string())?;

            // Close file
            self.builder
                .build_call(c_fclose, &[file_ptr.into()], "")
                .map_err(|e| e.to_string())?;

            self.builder
                .build_return(Some(&buf_ptr))
                .map_err(|e| e.to_string())?;

            self.builtin_functions.insert("readFile".to_string(), func);
            self.builtin_return_types
                .insert("readFile".to_string(), Type::Text);
        }

        // --- writeFile(path: ptr, content: ptr) -> i1 ---
        // Writes content to file, returns true on success
        {
            let i1_type = self.context.bool_type();
            let fn_type = i1_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
            let func = self.module.add_function("englang_writeFile", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let path = func.get_nth_param(0).unwrap().into_pointer_value();
            let content = func.get_nth_param(1).unwrap().into_pointer_value();

            // Create "w" mode string
            let mode_str = self
                .builder
                .build_global_string_ptr("w", "write_mode")
                .map_err(|e| e.to_string())?;

            // Open file for writing
            let file = self
                .builder
                .build_call(c_fopen, &[path.into(), mode_str.as_pointer_value().into()], "file")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let file_ptr = match file {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("fopen returned void".to_string()),
            };

            // Check if file is null
            let is_null = self
                .builder
                .build_is_null(file_ptr, "is_null")
                .map_err(|e| e.to_string())?;

            let null_bb = self.context.append_basic_block(func, "null_file");
            let valid_bb = self.context.append_basic_block(func, "valid_file");

            self.builder
                .build_conditional_branch(is_null, null_bb, valid_bb)
                .map_err(|e| e.to_string())?;

            // Null file - return false
            self.builder.position_at_end(null_bb);
            self.builder
                .build_return(Some(&i1_type.const_zero()))
                .map_err(|e| e.to_string())?;

            // Valid file - write contents
            self.builder.position_at_end(valid_bb);

            // Get content length using strlen
            let c_strlen = self
                .module
                .get_function("strlen")
                .ok_or("strlen not declared")?;
            let len = self
                .builder
                .build_call(c_strlen, &[content.into()], "len")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let content_len = match len {
                ValueKind::Basic(v) => v.into_int_value(),
                _ => return Err("strlen returned void".to_string()),
            };

            // Write content to file
            let written = self
                .builder
                .build_call(c_fwrite, &[content.into(), i64_type.const_int(1, false).into(), content_len.into(), file_ptr.into()], "written")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let bytes_written = match written {
                ValueKind::Basic(v) => v.into_int_value(),
                _ => return Err("fwrite returned void".to_string()),
            };

            // Close file
            self.builder
                .build_call(c_fclose, &[file_ptr.into()], "")
                .map_err(|e| e.to_string())?;

            // Return true if written == content_len
            let success = self
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::EQ,
                    bytes_written,
                    content_len,
                    "success",
                )
                .map_err(|e| e.to_string())?;

            self.builder
                .build_return(Some(&success))
                .map_err(|e| e.to_string())?;

            self.builtin_functions.insert("writeFile".to_string(), func);
            self.builtin_return_types
                .insert("writeFile".to_string(), Type::Bool);
        }

        // ==================== ARRAY FUNCTIONS ====================

        // --- zeros(n: i64) -> ptr (array of floats initialized to 0.0) ---
        {
            let fn_type = ptr_type.fn_type(&[i64_type.into()], false);
            let func = self.module.add_function("englang_zeros", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            let loop_cond = self.context.append_basic_block(func, "loop_cond");
            let loop_body = self.context.append_basic_block(func, "loop_body");
            let loop_end = self.context.append_basic_block(func, "loop_end");
            self.builder.position_at_end(entry);

            let n = func.get_nth_param(0).unwrap().into_int_value();

            // Allocate: 16 bytes header + n * 8 bytes for elements
            let header_size = i64_type.const_int(16, false);
            let elem_size = i64_type.const_int(8, false);
            let data_size = self.builder.build_int_mul(n, elem_size, "data_size").map_err(|e| e.to_string())?;
            let total_size = self.builder.build_int_add(header_size, data_size, "total_size").map_err(|e| e.to_string())?;

            let base_ptr = self.builder
                .build_call(malloc, &[total_size.into()], "base_ptr")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let base_ptr = match base_ptr {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("malloc returned void".to_string()),
            };

            // Store length at offset 0
            self.builder.build_store(base_ptr, n).map_err(|e| e.to_string())?;

            // Store capacity at offset 8
            let cap_ptr = unsafe {
                self.builder.build_gep(i8_type, base_ptr, &[i64_type.const_int(8, false)], "cap_ptr").map_err(|e| e.to_string())?
            };
            let cap_ptr = self.builder.build_pointer_cast(cap_ptr, ptr_type, "cap_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(cap_ptr, n).map_err(|e| e.to_string())?;

            // Data starts at offset 16
            let data_ptr = unsafe {
                self.builder.build_gep(i8_type, base_ptr, &[header_size], "data_ptr").map_err(|e| e.to_string())?
            };

            // Initialize all elements to 0.0
            let counter = self.builder.build_alloca(i64_type, "counter").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, i64_type.const_int(0, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_cond);
            let i = self.builder.build_load(i64_type, counter, "i").map_err(|e| e.to_string())?.into_int_value();
            let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i, n, "cond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cond, loop_body, loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_body);
            let offset = self.builder.build_int_mul(i, elem_size, "offset").map_err(|e| e.to_string())?;
            let elem_ptr = unsafe {
                self.builder.build_gep(i8_type, data_ptr, &[offset], "elem_ptr").map_err(|e| e.to_string())?
            };
            let elem_ptr = self.builder.build_pointer_cast(elem_ptr, ptr_type, "elem_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(elem_ptr, f64_type.const_float(0.0)).map_err(|e| e.to_string())?;
            let next_i = self.builder.build_int_add(i, i64_type.const_int(1, false), "next_i").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_i).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_end);
            self.builder.build_return(Some(&data_ptr)).map_err(|e| e.to_string())?;

            self.builtin_functions.insert("zeros".to_string(), func);
            self.builtin_return_types.insert("zeros".to_string(), Type::List(Box::new(Type::Float)));
        }

        // --- ones(n: i64) -> ptr (array of floats initialized to 1.0) ---
        {
            let fn_type = ptr_type.fn_type(&[i64_type.into()], false);
            let func = self.module.add_function("englang_ones", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            let loop_cond = self.context.append_basic_block(func, "loop_cond");
            let loop_body = self.context.append_basic_block(func, "loop_body");
            let loop_end = self.context.append_basic_block(func, "loop_end");
            self.builder.position_at_end(entry);

            let n = func.get_nth_param(0).unwrap().into_int_value();

            let header_size = i64_type.const_int(16, false);
            let elem_size = i64_type.const_int(8, false);
            let data_size = self.builder.build_int_mul(n, elem_size, "data_size").map_err(|e| e.to_string())?;
            let total_size = self.builder.build_int_add(header_size, data_size, "total_size").map_err(|e| e.to_string())?;

            let base_ptr = self.builder
                .build_call(malloc, &[total_size.into()], "base_ptr")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let base_ptr = match base_ptr {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("malloc returned void".to_string()),
            };

            self.builder.build_store(base_ptr, n).map_err(|e| e.to_string())?;
            let cap_ptr = unsafe {
                self.builder.build_gep(i8_type, base_ptr, &[i64_type.const_int(8, false)], "cap_ptr").map_err(|e| e.to_string())?
            };
            let cap_ptr = self.builder.build_pointer_cast(cap_ptr, ptr_type, "cap_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(cap_ptr, n).map_err(|e| e.to_string())?;

            let data_ptr = unsafe {
                self.builder.build_gep(i8_type, base_ptr, &[header_size], "data_ptr").map_err(|e| e.to_string())?
            };

            let counter = self.builder.build_alloca(i64_type, "counter").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, i64_type.const_int(0, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_cond);
            let i = self.builder.build_load(i64_type, counter, "i").map_err(|e| e.to_string())?.into_int_value();
            let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i, n, "cond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cond, loop_body, loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_body);
            let offset = self.builder.build_int_mul(i, elem_size, "offset").map_err(|e| e.to_string())?;
            let elem_ptr = unsafe {
                self.builder.build_gep(i8_type, data_ptr, &[offset], "elem_ptr").map_err(|e| e.to_string())?
            };
            let elem_ptr = self.builder.build_pointer_cast(elem_ptr, ptr_type, "elem_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(elem_ptr, f64_type.const_float(1.0)).map_err(|e| e.to_string())?;
            let next_i = self.builder.build_int_add(i, i64_type.const_int(1, false), "next_i").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_i).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_end);
            self.builder.build_return(Some(&data_ptr)).map_err(|e| e.to_string())?;

            self.builtin_functions.insert("ones".to_string(), func);
            self.builtin_return_types.insert("ones".to_string(), Type::List(Box::new(Type::Float)));
        }

        // --- range(start: i64, end: i64) -> ptr (array of ints from start to end-1) ---
        {
            let fn_type = ptr_type.fn_type(&[i64_type.into(), i64_type.into()], false);
            let func = self.module.add_function("englang_range", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            let loop_cond = self.context.append_basic_block(func, "loop_cond");
            let loop_body = self.context.append_basic_block(func, "loop_body");
            let loop_end = self.context.append_basic_block(func, "loop_end");
            self.builder.position_at_end(entry);

            let start = func.get_nth_param(0).unwrap().into_int_value();
            let end = func.get_nth_param(1).unwrap().into_int_value();

            // n = end - start
            let n = self.builder.build_int_sub(end, start, "n").map_err(|e| e.to_string())?;

            let header_size = i64_type.const_int(16, false);
            let elem_size = i64_type.const_int(8, false);
            let data_size = self.builder.build_int_mul(n, elem_size, "data_size").map_err(|e| e.to_string())?;
            let total_size = self.builder.build_int_add(header_size, data_size, "total_size").map_err(|e| e.to_string())?;

            let base_ptr = self.builder
                .build_call(malloc, &[total_size.into()], "base_ptr")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let base_ptr = match base_ptr {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("malloc returned void".to_string()),
            };

            self.builder.build_store(base_ptr, n).map_err(|e| e.to_string())?;
            let cap_ptr = unsafe {
                self.builder.build_gep(i8_type, base_ptr, &[i64_type.const_int(8, false)], "cap_ptr").map_err(|e| e.to_string())?
            };
            let cap_ptr = self.builder.build_pointer_cast(cap_ptr, ptr_type, "cap_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(cap_ptr, n).map_err(|e| e.to_string())?;

            let data_ptr = unsafe {
                self.builder.build_gep(i8_type, base_ptr, &[header_size], "data_ptr").map_err(|e| e.to_string())?
            };

            let counter = self.builder.build_alloca(i64_type, "counter").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, i64_type.const_int(0, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_cond);
            let i = self.builder.build_load(i64_type, counter, "i").map_err(|e| e.to_string())?.into_int_value();
            let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i, n, "cond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cond, loop_body, loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_body);
            // arr[i] = start + i
            let value = self.builder.build_int_add(start, i, "value").map_err(|e| e.to_string())?;
            let offset = self.builder.build_int_mul(i, elem_size, "offset").map_err(|e| e.to_string())?;
            let elem_ptr = unsafe {
                self.builder.build_gep(i8_type, data_ptr, &[offset], "elem_ptr").map_err(|e| e.to_string())?
            };
            let elem_ptr = self.builder.build_pointer_cast(elem_ptr, ptr_type, "elem_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(elem_ptr, value).map_err(|e| e.to_string())?;
            let next_i = self.builder.build_int_add(i, i64_type.const_int(1, false), "next_i").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_i).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_end);
            self.builder.build_return(Some(&data_ptr)).map_err(|e| e.to_string())?;

            self.builtin_functions.insert("range".to_string(), func);
            self.builtin_return_types.insert("range".to_string(), Type::List(Box::new(Type::Int)));
        }

        // --- arrayLength(arr: ptr) -> i64 ---
        {
            let fn_type = i64_type.fn_type(&[ptr_type.into()], false);
            let func = self.module.add_function("englang_arrayLength", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let arr = func.get_nth_param(0).unwrap().into_pointer_value();

            // Length is at offset -16 from data pointer
            let neg_16 = i64_type.const_int((-16i64) as u64, true);
            let len_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[neg_16], "len_ptr").map_err(|e| e.to_string())?
            };
            let len_ptr = self.builder.build_pointer_cast(len_ptr, ptr_type, "len_ptr_typed").map_err(|e| e.to_string())?;
            let len = self.builder.build_load(i64_type, len_ptr, "len").map_err(|e| e.to_string())?;

            self.builder.build_return(Some(&len)).map_err(|e| e.to_string())?;

            for name in &["arrayLength", "len"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types.insert(name.to_string(), Type::Int);
            }
        }

        // --- sum(arr: ptr) -> f64 ---
        {
            let fn_type = f64_type.fn_type(&[ptr_type.into()], false);
            let func = self.module.add_function("englang_sum", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            let loop_cond = self.context.append_basic_block(func, "loop_cond");
            let loop_body = self.context.append_basic_block(func, "loop_body");
            let loop_end = self.context.append_basic_block(func, "loop_end");
            self.builder.position_at_end(entry);

            let arr = func.get_nth_param(0).unwrap().into_pointer_value();

            // Get length from header
            let neg_16 = i64_type.const_int((-16i64) as u64, true);
            let len_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[neg_16], "len_ptr").map_err(|e| e.to_string())?
            };
            let len_ptr = self.builder.build_pointer_cast(len_ptr, ptr_type, "len_ptr_typed").map_err(|e| e.to_string())?;
            let len = self.builder.build_load(i64_type, len_ptr, "len").map_err(|e| e.to_string())?.into_int_value();

            let total = self.builder.build_alloca(f64_type, "total").map_err(|e| e.to_string())?;
            self.builder.build_store(total, f64_type.const_float(0.0)).map_err(|e| e.to_string())?;

            let counter = self.builder.build_alloca(i64_type, "counter").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, i64_type.const_int(0, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_cond);
            let i = self.builder.build_load(i64_type, counter, "i").map_err(|e| e.to_string())?.into_int_value();
            let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i, len, "cond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cond, loop_body, loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_body);
            let elem_size = i64_type.const_int(8, false);
            let offset = self.builder.build_int_mul(i, elem_size, "offset").map_err(|e| e.to_string())?;
            let elem_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[offset], "elem_ptr").map_err(|e| e.to_string())?
            };
            let elem_ptr = self.builder.build_pointer_cast(elem_ptr, ptr_type, "elem_ptr_typed").map_err(|e| e.to_string())?;
            let elem = self.builder.build_load(f64_type, elem_ptr, "elem").map_err(|e| e.to_string())?.into_float_value();
            let current = self.builder.build_load(f64_type, total, "current").map_err(|e| e.to_string())?.into_float_value();
            let new_total = self.builder.build_float_add(current, elem, "new_total").map_err(|e| e.to_string())?;
            self.builder.build_store(total, new_total).map_err(|e| e.to_string())?;
            let next_i = self.builder.build_int_add(i, i64_type.const_int(1, false), "next_i").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_i).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_end);
            let result = self.builder.build_load(f64_type, total, "result").map_err(|e| e.to_string())?;
            self.builder.build_return(Some(&result)).map_err(|e| e.to_string())?;

            self.builtin_functions.insert("sum".to_string(), func);
            self.builtin_return_types.insert("sum".to_string(), Type::Float);
        }

        // --- mean(arr: ptr) -> f64 ---
        {
            let fn_type = f64_type.fn_type(&[ptr_type.into()], false);
            let func = self.module.add_function("englang_mean", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            let loop_cond = self.context.append_basic_block(func, "loop_cond");
            let loop_body = self.context.append_basic_block(func, "loop_body");
            let loop_end = self.context.append_basic_block(func, "loop_end");
            self.builder.position_at_end(entry);

            let arr = func.get_nth_param(0).unwrap().into_pointer_value();

            let neg_16 = i64_type.const_int((-16i64) as u64, true);
            let len_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[neg_16], "len_ptr").map_err(|e| e.to_string())?
            };
            let len_ptr = self.builder.build_pointer_cast(len_ptr, ptr_type, "len_ptr_typed").map_err(|e| e.to_string())?;
            let len = self.builder.build_load(i64_type, len_ptr, "len").map_err(|e| e.to_string())?.into_int_value();

            let total = self.builder.build_alloca(f64_type, "total").map_err(|e| e.to_string())?;
            self.builder.build_store(total, f64_type.const_float(0.0)).map_err(|e| e.to_string())?;

            let counter = self.builder.build_alloca(i64_type, "counter").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, i64_type.const_int(0, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_cond);
            let i = self.builder.build_load(i64_type, counter, "i").map_err(|e| e.to_string())?.into_int_value();
            let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i, len, "cond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cond, loop_body, loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_body);
            let elem_size = i64_type.const_int(8, false);
            let offset = self.builder.build_int_mul(i, elem_size, "offset").map_err(|e| e.to_string())?;
            let elem_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[offset], "elem_ptr").map_err(|e| e.to_string())?
            };
            let elem_ptr = self.builder.build_pointer_cast(elem_ptr, ptr_type, "elem_ptr_typed").map_err(|e| e.to_string())?;
            let elem = self.builder.build_load(f64_type, elem_ptr, "elem").map_err(|e| e.to_string())?.into_float_value();
            let current = self.builder.build_load(f64_type, total, "current").map_err(|e| e.to_string())?.into_float_value();
            let new_total = self.builder.build_float_add(current, elem, "new_total").map_err(|e| e.to_string())?;
            self.builder.build_store(total, new_total).map_err(|e| e.to_string())?;
            let next_i = self.builder.build_int_add(i, i64_type.const_int(1, false), "next_i").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_i).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_end);
            let sum = self.builder.build_load(f64_type, total, "sum").map_err(|e| e.to_string())?.into_float_value();
            let len_float = self.builder.build_signed_int_to_float(len, f64_type, "len_float").map_err(|e| e.to_string())?;
            let mean = self.builder.build_float_div(sum, len_float, "mean").map_err(|e| e.to_string())?;
            self.builder.build_return(Some(&mean)).map_err(|e| e.to_string())?;

            for name in &["mean", "average"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types.insert(name.to_string(), Type::Float);
            }
        }

        // --- arrayMin(arr: ptr) -> f64 ---
        {
            let fn_type = f64_type.fn_type(&[ptr_type.into()], false);
            let func = self.module.add_function("englang_arrayMin", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            let loop_cond = self.context.append_basic_block(func, "loop_cond");
            let loop_body = self.context.append_basic_block(func, "loop_body");
            let loop_end = self.context.append_basic_block(func, "loop_end");
            self.builder.position_at_end(entry);

            let arr = func.get_nth_param(0).unwrap().into_pointer_value();

            let neg_16 = i64_type.const_int((-16i64) as u64, true);
            let len_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[neg_16], "len_ptr").map_err(|e| e.to_string())?
            };
            let len_ptr = self.builder.build_pointer_cast(len_ptr, ptr_type, "len_ptr_typed").map_err(|e| e.to_string())?;
            let len = self.builder.build_load(i64_type, len_ptr, "len").map_err(|e| e.to_string())?.into_int_value();

            // Initialize min to first element
            let first_ptr = self.builder.build_pointer_cast(arr, ptr_type, "first_ptr").map_err(|e| e.to_string())?;
            let first = self.builder.build_load(f64_type, first_ptr, "first").map_err(|e| e.to_string())?.into_float_value();
            let min_val = self.builder.build_alloca(f64_type, "min_val").map_err(|e| e.to_string())?;
            self.builder.build_store(min_val, first).map_err(|e| e.to_string())?;

            let counter = self.builder.build_alloca(i64_type, "counter").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, i64_type.const_int(1, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_cond);
            let i = self.builder.build_load(i64_type, counter, "i").map_err(|e| e.to_string())?.into_int_value();
            let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i, len, "cond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cond, loop_body, loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_body);
            let elem_size = i64_type.const_int(8, false);
            let offset = self.builder.build_int_mul(i, elem_size, "offset").map_err(|e| e.to_string())?;
            let elem_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[offset], "elem_ptr").map_err(|e| e.to_string())?
            };
            let elem_ptr = self.builder.build_pointer_cast(elem_ptr, ptr_type, "elem_ptr_typed").map_err(|e| e.to_string())?;
            let elem = self.builder.build_load(f64_type, elem_ptr, "elem").map_err(|e| e.to_string())?.into_float_value();
            let current_min = self.builder.build_load(f64_type, min_val, "current_min").map_err(|e| e.to_string())?.into_float_value();

            // Use fmin to get the smaller value
            let c_fmin = self.module.get_function("fmin").ok_or("fmin not declared")?;
            let new_min = self.builder.build_call(c_fmin, &[current_min.into(), elem.into()], "new_min")
                .map_err(|e| e.to_string())?.try_as_basic_value();
            let new_min = match new_min {
                ValueKind::Basic(v) => v.into_float_value(),
                _ => return Err("fmin returned void".to_string()),
            };
            self.builder.build_store(min_val, new_min).map_err(|e| e.to_string())?;

            let next_i = self.builder.build_int_add(i, i64_type.const_int(1, false), "next_i").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_i).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_end);
            let result = self.builder.build_load(f64_type, min_val, "result").map_err(|e| e.to_string())?;
            self.builder.build_return(Some(&result)).map_err(|e| e.to_string())?;

            for name in &["arrayMin", "minOf"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types.insert(name.to_string(), Type::Float);
            }
        }

        // --- arrayMax(arr: ptr) -> f64 ---
        {
            let fn_type = f64_type.fn_type(&[ptr_type.into()], false);
            let func = self.module.add_function("englang_arrayMax", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            let loop_cond = self.context.append_basic_block(func, "loop_cond");
            let loop_body = self.context.append_basic_block(func, "loop_body");
            let loop_end = self.context.append_basic_block(func, "loop_end");
            self.builder.position_at_end(entry);

            let arr = func.get_nth_param(0).unwrap().into_pointer_value();

            let neg_16 = i64_type.const_int((-16i64) as u64, true);
            let len_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[neg_16], "len_ptr").map_err(|e| e.to_string())?
            };
            let len_ptr = self.builder.build_pointer_cast(len_ptr, ptr_type, "len_ptr_typed").map_err(|e| e.to_string())?;
            let len = self.builder.build_load(i64_type, len_ptr, "len").map_err(|e| e.to_string())?.into_int_value();

            let first_ptr = self.builder.build_pointer_cast(arr, ptr_type, "first_ptr").map_err(|e| e.to_string())?;
            let first = self.builder.build_load(f64_type, first_ptr, "first").map_err(|e| e.to_string())?.into_float_value();
            let max_val = self.builder.build_alloca(f64_type, "max_val").map_err(|e| e.to_string())?;
            self.builder.build_store(max_val, first).map_err(|e| e.to_string())?;

            let counter = self.builder.build_alloca(i64_type, "counter").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, i64_type.const_int(1, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_cond);
            let i = self.builder.build_load(i64_type, counter, "i").map_err(|e| e.to_string())?.into_int_value();
            let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i, len, "cond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cond, loop_body, loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_body);
            let elem_size = i64_type.const_int(8, false);
            let offset = self.builder.build_int_mul(i, elem_size, "offset").map_err(|e| e.to_string())?;
            let elem_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[offset], "elem_ptr").map_err(|e| e.to_string())?
            };
            let elem_ptr = self.builder.build_pointer_cast(elem_ptr, ptr_type, "elem_ptr_typed").map_err(|e| e.to_string())?;
            let elem = self.builder.build_load(f64_type, elem_ptr, "elem").map_err(|e| e.to_string())?.into_float_value();
            let current_max = self.builder.build_load(f64_type, max_val, "current_max").map_err(|e| e.to_string())?.into_float_value();

            let c_fmax = self.module.get_function("fmax").ok_or("fmax not declared")?;
            let new_max = self.builder.build_call(c_fmax, &[current_max.into(), elem.into()], "new_max")
                .map_err(|e| e.to_string())?.try_as_basic_value();
            let new_max = match new_max {
                ValueKind::Basic(v) => v.into_float_value(),
                _ => return Err("fmax returned void".to_string()),
            };
            self.builder.build_store(max_val, new_max).map_err(|e| e.to_string())?;

            let next_i = self.builder.build_int_add(i, i64_type.const_int(1, false), "next_i").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_i).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_end);
            let result = self.builder.build_load(f64_type, max_val, "result").map_err(|e| e.to_string())?;
            self.builder.build_return(Some(&result)).map_err(|e| e.to_string())?;

            for name in &["arrayMax", "maxOf"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types.insert(name.to_string(), Type::Float);
            }
        }

        // --- append(arr: ptr, value: i64) -> ptr (new array with value appended) ---
        {
            let fn_type = ptr_type.fn_type(&[ptr_type.into(), i64_type.into()], false);
            let func = self.module.add_function("englang_append", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            let loop_cond = self.context.append_basic_block(func, "loop_cond");
            let loop_body = self.context.append_basic_block(func, "loop_body");
            let loop_end = self.context.append_basic_block(func, "loop_end");
            self.builder.position_at_end(entry);

            let arr = func.get_nth_param(0).unwrap().into_pointer_value();
            let value = func.get_nth_param(1).unwrap().into_int_value();

            // Get old length
            let neg_16 = i64_type.const_int((-16i64) as u64, true);
            let len_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[neg_16], "len_ptr").map_err(|e| e.to_string())?
            };
            let len_ptr = self.builder.build_pointer_cast(len_ptr, ptr_type, "len_ptr_typed").map_err(|e| e.to_string())?;
            let old_len = self.builder.build_load(i64_type, len_ptr, "old_len").map_err(|e| e.to_string())?.into_int_value();

            // New length = old_len + 1
            let new_len = self.builder.build_int_add(old_len, i64_type.const_int(1, false), "new_len").map_err(|e| e.to_string())?;

            // Allocate new array
            let header_size = i64_type.const_int(16, false);
            let elem_size = i64_type.const_int(8, false);
            let data_size = self.builder.build_int_mul(new_len, elem_size, "data_size").map_err(|e| e.to_string())?;
            let total_size = self.builder.build_int_add(header_size, data_size, "total_size").map_err(|e| e.to_string())?;

            let base_ptr = self.builder
                .build_call(malloc, &[total_size.into()], "base_ptr")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let base_ptr = match base_ptr {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("malloc returned void".to_string()),
            };

            // Store new length and capacity
            self.builder.build_store(base_ptr, new_len).map_err(|e| e.to_string())?;
            let cap_ptr = unsafe {
                self.builder.build_gep(i8_type, base_ptr, &[i64_type.const_int(8, false)], "cap_ptr").map_err(|e| e.to_string())?
            };
            let cap_ptr = self.builder.build_pointer_cast(cap_ptr, ptr_type, "cap_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(cap_ptr, new_len).map_err(|e| e.to_string())?;

            let new_data_ptr = unsafe {
                self.builder.build_gep(i8_type, base_ptr, &[header_size], "new_data_ptr").map_err(|e| e.to_string())?
            };

            // Copy old elements
            let counter = self.builder.build_alloca(i64_type, "counter").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, i64_type.const_int(0, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_cond);
            let i = self.builder.build_load(i64_type, counter, "i").map_err(|e| e.to_string())?.into_int_value();
            let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i, old_len, "cond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cond, loop_body, loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_body);
            let offset = self.builder.build_int_mul(i, elem_size, "offset").map_err(|e| e.to_string())?;
            let src_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[offset], "src_ptr").map_err(|e| e.to_string())?
            };
            let src_ptr = self.builder.build_pointer_cast(src_ptr, ptr_type, "src_ptr_typed").map_err(|e| e.to_string())?;
            let elem = self.builder.build_load(i64_type, src_ptr, "elem").map_err(|e| e.to_string())?;
            let dst_ptr = unsafe {
                self.builder.build_gep(i8_type, new_data_ptr, &[offset], "dst_ptr").map_err(|e| e.to_string())?
            };
            let dst_ptr = self.builder.build_pointer_cast(dst_ptr, ptr_type, "dst_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(dst_ptr, elem).map_err(|e| e.to_string())?;
            let next_i = self.builder.build_int_add(i, i64_type.const_int(1, false), "next_i").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_i).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_end);
            // Append new value at end
            let last_offset = self.builder.build_int_mul(old_len, elem_size, "last_offset").map_err(|e| e.to_string())?;
            let last_ptr = unsafe {
                self.builder.build_gep(i8_type, new_data_ptr, &[last_offset], "last_ptr").map_err(|e| e.to_string())?
            };
            let last_ptr = self.builder.build_pointer_cast(last_ptr, ptr_type, "last_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(last_ptr, value).map_err(|e| e.to_string())?;

            self.builder.build_return(Some(&new_data_ptr)).map_err(|e| e.to_string())?;

            for name in &["append", "push"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types.insert(name.to_string(), Type::List(Box::new(Type::Int)));
            }
        }

        // --- reverse(arr: ptr) -> ptr (new reversed array) ---
        {
            let fn_type = ptr_type.fn_type(&[ptr_type.into()], false);
            let func = self.module.add_function("englang_reverse", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            let loop_cond = self.context.append_basic_block(func, "loop_cond");
            let loop_body = self.context.append_basic_block(func, "loop_body");
            let loop_end = self.context.append_basic_block(func, "loop_end");
            self.builder.position_at_end(entry);

            let arr = func.get_nth_param(0).unwrap().into_pointer_value();

            // Get length
            let neg_16 = i64_type.const_int((-16i64) as u64, true);
            let len_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[neg_16], "len_ptr").map_err(|e| e.to_string())?
            };
            let len_ptr = self.builder.build_pointer_cast(len_ptr, ptr_type, "len_ptr_typed").map_err(|e| e.to_string())?;
            let len = self.builder.build_load(i64_type, len_ptr, "len").map_err(|e| e.to_string())?.into_int_value();

            // Allocate new array
            let header_size = i64_type.const_int(16, false);
            let elem_size = i64_type.const_int(8, false);
            let data_size = self.builder.build_int_mul(len, elem_size, "data_size").map_err(|e| e.to_string())?;
            let total_size = self.builder.build_int_add(header_size, data_size, "total_size").map_err(|e| e.to_string())?;

            let base_ptr = self.builder
                .build_call(malloc, &[total_size.into()], "base_ptr")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let base_ptr = match base_ptr {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("malloc returned void".to_string()),
            };

            self.builder.build_store(base_ptr, len).map_err(|e| e.to_string())?;
            let cap_ptr = unsafe {
                self.builder.build_gep(i8_type, base_ptr, &[i64_type.const_int(8, false)], "cap_ptr").map_err(|e| e.to_string())?
            };
            let cap_ptr = self.builder.build_pointer_cast(cap_ptr, ptr_type, "cap_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(cap_ptr, len).map_err(|e| e.to_string())?;

            let new_data_ptr = unsafe {
                self.builder.build_gep(i8_type, base_ptr, &[header_size], "new_data_ptr").map_err(|e| e.to_string())?
            };

            // Copy elements in reverse order
            let counter = self.builder.build_alloca(i64_type, "counter").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, i64_type.const_int(0, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_cond);
            let i = self.builder.build_load(i64_type, counter, "i").map_err(|e| e.to_string())?.into_int_value();
            let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i, len, "cond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cond, loop_body, loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_body);
            // src_idx = len - 1 - i
            let len_minus_1 = self.builder.build_int_sub(len, i64_type.const_int(1, false), "len_minus_1").map_err(|e| e.to_string())?;
            let src_idx = self.builder.build_int_sub(len_minus_1, i, "src_idx").map_err(|e| e.to_string())?;
            let src_offset = self.builder.build_int_mul(src_idx, elem_size, "src_offset").map_err(|e| e.to_string())?;
            let src_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[src_offset], "src_ptr").map_err(|e| e.to_string())?
            };
            let src_ptr = self.builder.build_pointer_cast(src_ptr, ptr_type, "src_ptr_typed").map_err(|e| e.to_string())?;
            let elem = self.builder.build_load(i64_type, src_ptr, "elem").map_err(|e| e.to_string())?;

            let dst_offset = self.builder.build_int_mul(i, elem_size, "dst_offset").map_err(|e| e.to_string())?;
            let dst_ptr = unsafe {
                self.builder.build_gep(i8_type, new_data_ptr, &[dst_offset], "dst_ptr").map_err(|e| e.to_string())?
            };
            let dst_ptr = self.builder.build_pointer_cast(dst_ptr, ptr_type, "dst_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(dst_ptr, elem).map_err(|e| e.to_string())?;

            let next_i = self.builder.build_int_add(i, i64_type.const_int(1, false), "next_i").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_i).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_end);
            self.builder.build_return(Some(&new_data_ptr)).map_err(|e| e.to_string())?;

            self.builtin_functions.insert("reverse".to_string(), func);
            self.builtin_return_types.insert("reverse".to_string(), Type::List(Box::new(Type::Int)));
        }

        // ==================== ML/STATISTICS FUNCTIONS ====================

        // --- variance(arr: ptr) -> f64 ---
        // Computes: sum((x - mean)^2) / n
        {
            let fn_type = f64_type.fn_type(&[ptr_type.into()], false);
            let func = self.module.add_function("englang_variance", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            let mean_loop_cond = self.context.append_basic_block(func, "mean_loop_cond");
            let mean_loop_body = self.context.append_basic_block(func, "mean_loop_body");
            let mean_loop_end = self.context.append_basic_block(func, "mean_loop_end");
            let var_loop_cond = self.context.append_basic_block(func, "var_loop_cond");
            let var_loop_body = self.context.append_basic_block(func, "var_loop_body");
            let var_loop_end = self.context.append_basic_block(func, "var_loop_end");
            self.builder.position_at_end(entry);

            let arr = func.get_nth_param(0).unwrap().into_pointer_value();

            // Get length from header
            let neg_16 = i64_type.const_int((-16i64) as u64, true);
            let len_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[neg_16], "len_ptr").map_err(|e| e.to_string())?
            };
            let len_ptr = self.builder.build_pointer_cast(len_ptr, ptr_type, "len_ptr_typed").map_err(|e| e.to_string())?;
            let len = self.builder.build_load(i64_type, len_ptr, "len").map_err(|e| e.to_string())?.into_int_value();

            // Pass 1: Calculate mean
            let sum = self.builder.build_alloca(f64_type, "sum").map_err(|e| e.to_string())?;
            self.builder.build_store(sum, f64_type.const_float(0.0)).map_err(|e| e.to_string())?;
            let counter = self.builder.build_alloca(i64_type, "counter").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, i64_type.const_int(0, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(mean_loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(mean_loop_cond);
            let i = self.builder.build_load(i64_type, counter, "i").map_err(|e| e.to_string())?.into_int_value();
            let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i, len, "cond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cond, mean_loop_body, mean_loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(mean_loop_body);
            let elem_size = i64_type.const_int(8, false);
            let offset = self.builder.build_int_mul(i, elem_size, "offset").map_err(|e| e.to_string())?;
            let elem_ptr = unsafe {
                self.builder.build_gep(i8_type, arr, &[offset], "elem_ptr").map_err(|e| e.to_string())?
            };
            let elem_ptr = self.builder.build_pointer_cast(elem_ptr, ptr_type, "elem_ptr_typed").map_err(|e| e.to_string())?;
            let elem = self.builder.build_load(f64_type, elem_ptr, "elem").map_err(|e| e.to_string())?.into_float_value();
            let current_sum = self.builder.build_load(f64_type, sum, "current_sum").map_err(|e| e.to_string())?.into_float_value();
            let new_sum = self.builder.build_float_add(current_sum, elem, "new_sum").map_err(|e| e.to_string())?;
            self.builder.build_store(sum, new_sum).map_err(|e| e.to_string())?;
            let next_i = self.builder.build_int_add(i, i64_type.const_int(1, false), "next_i").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_i).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(mean_loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(mean_loop_end);
            let total = self.builder.build_load(f64_type, sum, "total").map_err(|e| e.to_string())?.into_float_value();
            let len_float = self.builder.build_signed_int_to_float(len, f64_type, "len_float").map_err(|e| e.to_string())?;
            let mean = self.builder.build_float_div(total, len_float, "mean").map_err(|e| e.to_string())?;

            // Pass 2: Calculate sum of squared differences
            let sq_sum = self.builder.build_alloca(f64_type, "sq_sum").map_err(|e| e.to_string())?;
            self.builder.build_store(sq_sum, f64_type.const_float(0.0)).map_err(|e| e.to_string())?;
            self.builder.build_store(counter, i64_type.const_int(0, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(var_loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(var_loop_cond);
            let i2 = self.builder.build_load(i64_type, counter, "i2").map_err(|e| e.to_string())?.into_int_value();
            let cond2 = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i2, len, "cond2").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cond2, var_loop_body, var_loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(var_loop_body);
            let offset2 = self.builder.build_int_mul(i2, elem_size, "offset2").map_err(|e| e.to_string())?;
            let elem_ptr2 = unsafe {
                self.builder.build_gep(i8_type, arr, &[offset2], "elem_ptr2").map_err(|e| e.to_string())?
            };
            let elem_ptr2 = self.builder.build_pointer_cast(elem_ptr2, ptr_type, "elem_ptr2_typed").map_err(|e| e.to_string())?;
            let elem2 = self.builder.build_load(f64_type, elem_ptr2, "elem2").map_err(|e| e.to_string())?.into_float_value();
            let diff = self.builder.build_float_sub(elem2, mean, "diff").map_err(|e| e.to_string())?;
            let diff_sq = self.builder.build_float_mul(diff, diff, "diff_sq").map_err(|e| e.to_string())?;
            let current_sq_sum = self.builder.build_load(f64_type, sq_sum, "current_sq_sum").map_err(|e| e.to_string())?.into_float_value();
            let new_sq_sum = self.builder.build_float_add(current_sq_sum, diff_sq, "new_sq_sum").map_err(|e| e.to_string())?;
            self.builder.build_store(sq_sum, new_sq_sum).map_err(|e| e.to_string())?;
            let next_i2 = self.builder.build_int_add(i2, i64_type.const_int(1, false), "next_i2").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_i2).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(var_loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(var_loop_end);
            let total_sq = self.builder.build_load(f64_type, sq_sum, "total_sq").map_err(|e| e.to_string())?.into_float_value();
            let variance = self.builder.build_float_div(total_sq, len_float, "variance").map_err(|e| e.to_string())?;
            self.builder.build_return(Some(&variance)).map_err(|e| e.to_string())?;

            for name in &["variance", "var"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types.insert(name.to_string(), Type::Float);
            }
        }

        // --- standardDeviation(arr: ptr) -> f64 ---
        // Computes: sqrt(variance(arr))
        {
            let fn_type = f64_type.fn_type(&[ptr_type.into()], false);
            let func = self.module.add_function("englang_stdDev", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let arr = func.get_nth_param(0).unwrap().into_pointer_value();

            // Call variance function
            let variance_fn = self.module.get_function("englang_variance").ok_or("variance not declared")?;
            let var_result = self.builder.build_call(variance_fn, &[arr.into()], "var_result")
                .map_err(|e| e.to_string())?.try_as_basic_value();
            let variance = match var_result {
                ValueKind::Basic(v) => v.into_float_value(),
                _ => return Err("variance returned void".to_string()),
            };

            // sqrt(variance)
            let c_sqrt = self.module.get_function("sqrt").ok_or("sqrt not declared")?;
            let result = self.builder.build_call(c_sqrt, &[variance.into()], "stddev")
                .map_err(|e| e.to_string())?.try_as_basic_value();
            let stddev = match result {
                ValueKind::Basic(v) => v.into_float_value(),
                _ => return Err("sqrt returned void".to_string()),
            };

            self.builder.build_return(Some(&stddev)).map_err(|e| e.to_string())?;

            for name in &["standardDeviation", "stdDev", "std"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types.insert(name.to_string(), Type::Float);
            }
        }

        // --- correlation(x: ptr, y: ptr) -> f64 ---
        // Pearson correlation coefficient
        {
            let fn_type = f64_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
            let func = self.module.add_function("englang_correlation", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            let loop_cond = self.context.append_basic_block(func, "loop_cond");
            let loop_body = self.context.append_basic_block(func, "loop_body");
            let loop_end = self.context.append_basic_block(func, "loop_end");
            self.builder.position_at_end(entry);

            let x_arr = func.get_nth_param(0).unwrap().into_pointer_value();
            let y_arr = func.get_nth_param(1).unwrap().into_pointer_value();

            // Get length from x (assume same length)
            let neg_16 = i64_type.const_int((-16i64) as u64, true);
            let len_ptr = unsafe {
                self.builder.build_gep(i8_type, x_arr, &[neg_16], "len_ptr").map_err(|e| e.to_string())?
            };
            let len_ptr = self.builder.build_pointer_cast(len_ptr, ptr_type, "len_ptr_typed").map_err(|e| e.to_string())?;
            let len = self.builder.build_load(i64_type, len_ptr, "len").map_err(|e| e.to_string())?.into_int_value();

            // Allocate accumulators
            let sum_x = self.builder.build_alloca(f64_type, "sum_x").map_err(|e| e.to_string())?;
            let sum_y = self.builder.build_alloca(f64_type, "sum_y").map_err(|e| e.to_string())?;
            let sum_xy = self.builder.build_alloca(f64_type, "sum_xy").map_err(|e| e.to_string())?;
            let sum_x2 = self.builder.build_alloca(f64_type, "sum_x2").map_err(|e| e.to_string())?;
            let sum_y2 = self.builder.build_alloca(f64_type, "sum_y2").map_err(|e| e.to_string())?;
            self.builder.build_store(sum_x, f64_type.const_float(0.0)).map_err(|e| e.to_string())?;
            self.builder.build_store(sum_y, f64_type.const_float(0.0)).map_err(|e| e.to_string())?;
            self.builder.build_store(sum_xy, f64_type.const_float(0.0)).map_err(|e| e.to_string())?;
            self.builder.build_store(sum_x2, f64_type.const_float(0.0)).map_err(|e| e.to_string())?;
            self.builder.build_store(sum_y2, f64_type.const_float(0.0)).map_err(|e| e.to_string())?;

            let counter = self.builder.build_alloca(i64_type, "counter").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, i64_type.const_int(0, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_cond);
            let i = self.builder.build_load(i64_type, counter, "i").map_err(|e| e.to_string())?.into_int_value();
            let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i, len, "cond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cond, loop_body, loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_body);
            let elem_size = i64_type.const_int(8, false);
            let offset = self.builder.build_int_mul(i, elem_size, "offset").map_err(|e| e.to_string())?;

            // Load x[i]
            let x_ptr = unsafe {
                self.builder.build_gep(i8_type, x_arr, &[offset], "x_ptr").map_err(|e| e.to_string())?
            };
            let x_ptr = self.builder.build_pointer_cast(x_ptr, ptr_type, "x_ptr_typed").map_err(|e| e.to_string())?;
            let x_val = self.builder.build_load(f64_type, x_ptr, "x_val").map_err(|e| e.to_string())?.into_float_value();

            // Load y[i]
            let y_ptr = unsafe {
                self.builder.build_gep(i8_type, y_arr, &[offset], "y_ptr").map_err(|e| e.to_string())?
            };
            let y_ptr = self.builder.build_pointer_cast(y_ptr, ptr_type, "y_ptr_typed").map_err(|e| e.to_string())?;
            let y_val = self.builder.build_load(f64_type, y_ptr, "y_val").map_err(|e| e.to_string())?.into_float_value();

            // Accumulate sums
            let cur_sum_x = self.builder.build_load(f64_type, sum_x, "cur_sum_x").map_err(|e| e.to_string())?.into_float_value();
            let new_sum_x = self.builder.build_float_add(cur_sum_x, x_val, "new_sum_x").map_err(|e| e.to_string())?;
            self.builder.build_store(sum_x, new_sum_x).map_err(|e| e.to_string())?;

            let cur_sum_y = self.builder.build_load(f64_type, sum_y, "cur_sum_y").map_err(|e| e.to_string())?.into_float_value();
            let new_sum_y = self.builder.build_float_add(cur_sum_y, y_val, "new_sum_y").map_err(|e| e.to_string())?;
            self.builder.build_store(sum_y, new_sum_y).map_err(|e| e.to_string())?;

            let xy = self.builder.build_float_mul(x_val, y_val, "xy").map_err(|e| e.to_string())?;
            let cur_sum_xy = self.builder.build_load(f64_type, sum_xy, "cur_sum_xy").map_err(|e| e.to_string())?.into_float_value();
            let new_sum_xy = self.builder.build_float_add(cur_sum_xy, xy, "new_sum_xy").map_err(|e| e.to_string())?;
            self.builder.build_store(sum_xy, new_sum_xy).map_err(|e| e.to_string())?;

            let x2 = self.builder.build_float_mul(x_val, x_val, "x2").map_err(|e| e.to_string())?;
            let cur_sum_x2 = self.builder.build_load(f64_type, sum_x2, "cur_sum_x2").map_err(|e| e.to_string())?.into_float_value();
            let new_sum_x2 = self.builder.build_float_add(cur_sum_x2, x2, "new_sum_x2").map_err(|e| e.to_string())?;
            self.builder.build_store(sum_x2, new_sum_x2).map_err(|e| e.to_string())?;

            let y2 = self.builder.build_float_mul(y_val, y_val, "y2").map_err(|e| e.to_string())?;
            let cur_sum_y2 = self.builder.build_load(f64_type, sum_y2, "cur_sum_y2").map_err(|e| e.to_string())?.into_float_value();
            let new_sum_y2 = self.builder.build_float_add(cur_sum_y2, y2, "new_sum_y2").map_err(|e| e.to_string())?;
            self.builder.build_store(sum_y2, new_sum_y2).map_err(|e| e.to_string())?;

            let next_i = self.builder.build_int_add(i, i64_type.const_int(1, false), "next_i").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_i).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_end);
            let n = self.builder.build_signed_int_to_float(len, f64_type, "n").map_err(|e| e.to_string())?;
            let sx = self.builder.build_load(f64_type, sum_x, "sx").map_err(|e| e.to_string())?.into_float_value();
            let sy = self.builder.build_load(f64_type, sum_y, "sy").map_err(|e| e.to_string())?.into_float_value();
            let sxy = self.builder.build_load(f64_type, sum_xy, "sxy").map_err(|e| e.to_string())?.into_float_value();
            let sx2 = self.builder.build_load(f64_type, sum_x2, "sx2").map_err(|e| e.to_string())?.into_float_value();
            let sy2 = self.builder.build_load(f64_type, sum_y2, "sy2").map_err(|e| e.to_string())?.into_float_value();

            // numerator = n * sum_xy - sum_x * sum_y
            let n_sxy = self.builder.build_float_mul(n, sxy, "n_sxy").map_err(|e| e.to_string())?;
            let sx_sy = self.builder.build_float_mul(sx, sy, "sx_sy").map_err(|e| e.to_string())?;
            let numerator = self.builder.build_float_sub(n_sxy, sx_sy, "numerator").map_err(|e| e.to_string())?;

            // denom_x = n * sum_x2 - sum_x^2
            let n_sx2 = self.builder.build_float_mul(n, sx2, "n_sx2").map_err(|e| e.to_string())?;
            let sx_sq = self.builder.build_float_mul(sx, sx, "sx_sq").map_err(|e| e.to_string())?;
            let denom_x = self.builder.build_float_sub(n_sx2, sx_sq, "denom_x").map_err(|e| e.to_string())?;

            // denom_y = n * sum_y2 - sum_y^2
            let n_sy2 = self.builder.build_float_mul(n, sy2, "n_sy2").map_err(|e| e.to_string())?;
            let sy_sq = self.builder.build_float_mul(sy, sy, "sy_sq").map_err(|e| e.to_string())?;
            let denom_y = self.builder.build_float_sub(n_sy2, sy_sq, "denom_y").map_err(|e| e.to_string())?;

            // denominator = sqrt(denom_x * denom_y)
            let denom_prod = self.builder.build_float_mul(denom_x, denom_y, "denom_prod").map_err(|e| e.to_string())?;
            let c_sqrt = self.module.get_function("sqrt").ok_or("sqrt not declared")?;
            let denominator = self.builder.build_call(c_sqrt, &[denom_prod.into()], "denominator")
                .map_err(|e| e.to_string())?.try_as_basic_value();
            let denominator = match denominator {
                ValueKind::Basic(v) => v.into_float_value(),
                _ => return Err("sqrt returned void".to_string()),
            };

            let corr = self.builder.build_float_div(numerator, denominator, "corr").map_err(|e| e.to_string())?;
            self.builder.build_return(Some(&corr)).map_err(|e| e.to_string())?;

            for name in &["correlation", "corr"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types.insert(name.to_string(), Type::Float);
            }
        }

        // --- fitLine(x: ptr, y: ptr) -> ptr (array with [slope, intercept]) ---
        // Least squares linear regression
        {
            let fn_type = ptr_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
            let func = self.module.add_function("englang_fitLine", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            let loop_cond = self.context.append_basic_block(func, "loop_cond");
            let loop_body = self.context.append_basic_block(func, "loop_body");
            let loop_end = self.context.append_basic_block(func, "loop_end");
            self.builder.position_at_end(entry);

            let x_arr = func.get_nth_param(0).unwrap().into_pointer_value();
            let y_arr = func.get_nth_param(1).unwrap().into_pointer_value();

            // Get length
            let neg_16 = i64_type.const_int((-16i64) as u64, true);
            let len_ptr = unsafe {
                self.builder.build_gep(i8_type, x_arr, &[neg_16], "len_ptr").map_err(|e| e.to_string())?
            };
            let len_ptr = self.builder.build_pointer_cast(len_ptr, ptr_type, "len_ptr_typed").map_err(|e| e.to_string())?;
            let len = self.builder.build_load(i64_type, len_ptr, "len").map_err(|e| e.to_string())?.into_int_value();

            // Accumulate sums
            let sum_x = self.builder.build_alloca(f64_type, "sum_x").map_err(|e| e.to_string())?;
            let sum_y = self.builder.build_alloca(f64_type, "sum_y").map_err(|e| e.to_string())?;
            let sum_xy = self.builder.build_alloca(f64_type, "sum_xy").map_err(|e| e.to_string())?;
            let sum_x2 = self.builder.build_alloca(f64_type, "sum_x2").map_err(|e| e.to_string())?;
            self.builder.build_store(sum_x, f64_type.const_float(0.0)).map_err(|e| e.to_string())?;
            self.builder.build_store(sum_y, f64_type.const_float(0.0)).map_err(|e| e.to_string())?;
            self.builder.build_store(sum_xy, f64_type.const_float(0.0)).map_err(|e| e.to_string())?;
            self.builder.build_store(sum_x2, f64_type.const_float(0.0)).map_err(|e| e.to_string())?;

            let counter = self.builder.build_alloca(i64_type, "counter").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, i64_type.const_int(0, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_cond);
            let i = self.builder.build_load(i64_type, counter, "i").map_err(|e| e.to_string())?.into_int_value();
            let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i, len, "cond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cond, loop_body, loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_body);
            let elem_size = i64_type.const_int(8, false);
            let offset = self.builder.build_int_mul(i, elem_size, "offset").map_err(|e| e.to_string())?;

            let x_ptr = unsafe {
                self.builder.build_gep(i8_type, x_arr, &[offset], "x_ptr").map_err(|e| e.to_string())?
            };
            let x_ptr = self.builder.build_pointer_cast(x_ptr, ptr_type, "x_ptr_typed").map_err(|e| e.to_string())?;
            let x_val = self.builder.build_load(f64_type, x_ptr, "x_val").map_err(|e| e.to_string())?.into_float_value();

            let y_ptr = unsafe {
                self.builder.build_gep(i8_type, y_arr, &[offset], "y_ptr").map_err(|e| e.to_string())?
            };
            let y_ptr = self.builder.build_pointer_cast(y_ptr, ptr_type, "y_ptr_typed").map_err(|e| e.to_string())?;
            let y_val = self.builder.build_load(f64_type, y_ptr, "y_val").map_err(|e| e.to_string())?.into_float_value();

            let cur_sum_x = self.builder.build_load(f64_type, sum_x, "cur_sum_x").map_err(|e| e.to_string())?.into_float_value();
            let new_sum_x = self.builder.build_float_add(cur_sum_x, x_val, "new_sum_x").map_err(|e| e.to_string())?;
            self.builder.build_store(sum_x, new_sum_x).map_err(|e| e.to_string())?;

            let cur_sum_y = self.builder.build_load(f64_type, sum_y, "cur_sum_y").map_err(|e| e.to_string())?.into_float_value();
            let new_sum_y = self.builder.build_float_add(cur_sum_y, y_val, "new_sum_y").map_err(|e| e.to_string())?;
            self.builder.build_store(sum_y, new_sum_y).map_err(|e| e.to_string())?;

            let xy = self.builder.build_float_mul(x_val, y_val, "xy").map_err(|e| e.to_string())?;
            let cur_sum_xy = self.builder.build_load(f64_type, sum_xy, "cur_sum_xy").map_err(|e| e.to_string())?.into_float_value();
            let new_sum_xy = self.builder.build_float_add(cur_sum_xy, xy, "new_sum_xy").map_err(|e| e.to_string())?;
            self.builder.build_store(sum_xy, new_sum_xy).map_err(|e| e.to_string())?;

            let x2 = self.builder.build_float_mul(x_val, x_val, "x2").map_err(|e| e.to_string())?;
            let cur_sum_x2 = self.builder.build_load(f64_type, sum_x2, "cur_sum_x2").map_err(|e| e.to_string())?.into_float_value();
            let new_sum_x2 = self.builder.build_float_add(cur_sum_x2, x2, "new_sum_x2").map_err(|e| e.to_string())?;
            self.builder.build_store(sum_x2, new_sum_x2).map_err(|e| e.to_string())?;

            let next_i = self.builder.build_int_add(i, i64_type.const_int(1, false), "next_i").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_i).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(loop_end);
            let n = self.builder.build_signed_int_to_float(len, f64_type, "n").map_err(|e| e.to_string())?;
            let sx = self.builder.build_load(f64_type, sum_x, "sx").map_err(|e| e.to_string())?.into_float_value();
            let sy = self.builder.build_load(f64_type, sum_y, "sy").map_err(|e| e.to_string())?.into_float_value();
            let sxy = self.builder.build_load(f64_type, sum_xy, "sxy").map_err(|e| e.to_string())?.into_float_value();
            let sx2 = self.builder.build_load(f64_type, sum_x2, "sx2").map_err(|e| e.to_string())?.into_float_value();

            // slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x^2)
            let n_sxy = self.builder.build_float_mul(n, sxy, "n_sxy").map_err(|e| e.to_string())?;
            let sx_sy = self.builder.build_float_mul(sx, sy, "sx_sy").map_err(|e| e.to_string())?;
            let numer = self.builder.build_float_sub(n_sxy, sx_sy, "numer").map_err(|e| e.to_string())?;
            let n_sx2 = self.builder.build_float_mul(n, sx2, "n_sx2").map_err(|e| e.to_string())?;
            let sx_sq = self.builder.build_float_mul(sx, sx, "sx_sq").map_err(|e| e.to_string())?;
            let denom = self.builder.build_float_sub(n_sx2, sx_sq, "denom").map_err(|e| e.to_string())?;
            let slope = self.builder.build_float_div(numer, denom, "slope").map_err(|e| e.to_string())?;

            // intercept = (sum_y - slope * sum_x) / n
            let slope_sx = self.builder.build_float_mul(slope, sx, "slope_sx").map_err(|e| e.to_string())?;
            let sy_minus = self.builder.build_float_sub(sy, slope_sx, "sy_minus").map_err(|e| e.to_string())?;
            let intercept = self.builder.build_float_div(sy_minus, n, "intercept").map_err(|e| e.to_string())?;

            // Allocate result array [slope, intercept]
            let header_size = i64_type.const_int(16, false);
            let elem_size_const = i64_type.const_int(8, false);
            let data_size = i64_type.const_int(16, false); // 2 * 8
            let total_size = self.builder.build_int_add(header_size, data_size, "total_size").map_err(|e| e.to_string())?;
            let base_ptr = self.builder.build_call(malloc, &[total_size.into()], "base_ptr")
                .map_err(|e| e.to_string())?.try_as_basic_value();
            let base_ptr = match base_ptr {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("malloc returned void".to_string()),
            };

            // Store length = 2
            self.builder.build_store(base_ptr, i64_type.const_int(2, false)).map_err(|e| e.to_string())?;
            let cap_ptr = unsafe {
                self.builder.build_gep(i8_type, base_ptr, &[i64_type.const_int(8, false)], "cap_ptr").map_err(|e| e.to_string())?
            };
            let cap_ptr = self.builder.build_pointer_cast(cap_ptr, ptr_type, "cap_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(cap_ptr, i64_type.const_int(2, false)).map_err(|e| e.to_string())?;

            // Data pointer
            let data_ptr = unsafe {
                self.builder.build_gep(i8_type, base_ptr, &[header_size], "data_ptr").map_err(|e| e.to_string())?
            };

            // Store slope at offset 0
            let slope_ptr = self.builder.build_pointer_cast(data_ptr, ptr_type, "slope_ptr").map_err(|e| e.to_string())?;
            self.builder.build_store(slope_ptr, slope).map_err(|e| e.to_string())?;

            // Store intercept at offset 8
            let intercept_ptr = unsafe {
                self.builder.build_gep(i8_type, data_ptr, &[elem_size_const], "intercept_ptr").map_err(|e| e.to_string())?
            };
            let intercept_ptr = self.builder.build_pointer_cast(intercept_ptr, ptr_type, "intercept_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(intercept_ptr, intercept).map_err(|e| e.to_string())?;

            self.builder.build_return(Some(&data_ptr)).map_err(|e| e.to_string())?;

            for name in &["fitLine", "linearRegression"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types.insert(name.to_string(), Type::List(Box::new(Type::Float)));
            }
        }

        // --- predictLinear(coeffs: ptr, x: f64) -> f64 ---
        // Returns slope * x + intercept
        {
            let fn_type = f64_type.fn_type(&[ptr_type.into(), f64_type.into()], false);
            let func = self.module.add_function("englang_predictLinear", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let coeffs = func.get_nth_param(0).unwrap().into_pointer_value();
            let x = func.get_nth_param(1).unwrap().into_float_value();

            // Load slope (at offset 0)
            let slope_ptr = self.builder.build_pointer_cast(coeffs, ptr_type, "slope_ptr").map_err(|e| e.to_string())?;
            let slope = self.builder.build_load(f64_type, slope_ptr, "slope").map_err(|e| e.to_string())?.into_float_value();

            // Load intercept (at offset 8)
            let elem_size = i64_type.const_int(8, false);
            let intercept_ptr = unsafe {
                self.builder.build_gep(i8_type, coeffs, &[elem_size], "intercept_ptr").map_err(|e| e.to_string())?
            };
            let intercept_ptr = self.builder.build_pointer_cast(intercept_ptr, ptr_type, "intercept_ptr_typed").map_err(|e| e.to_string())?;
            let intercept = self.builder.build_load(f64_type, intercept_ptr, "intercept").map_err(|e| e.to_string())?.into_float_value();

            // result = slope * x + intercept
            let slope_x = self.builder.build_float_mul(slope, x, "slope_x").map_err(|e| e.to_string())?;
            let result = self.builder.build_float_add(slope_x, intercept, "result").map_err(|e| e.to_string())?;

            self.builder.build_return(Some(&result)).map_err(|e| e.to_string())?;

            self.builtin_functions.insert("predictLinear".to_string(), func);
            self.builtin_return_types.insert("predictLinear".to_string(), Type::Float);
        }

        // --- kMeans(data: ptr, k: i64) -> ptr (cluster assignments) ---
        // Simplified 1D k-means clustering using Lloyd's algorithm
        {
            let fn_type = ptr_type.fn_type(&[ptr_type.into(), i64_type.into()], false);
            let func = self.module.add_function("englang_kMeans", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            let init_loop_cond = self.context.append_basic_block(func, "init_loop_cond");
            let init_loop_body = self.context.append_basic_block(func, "init_loop_body");
            let init_loop_end = self.context.append_basic_block(func, "init_loop_end");
            let iter_loop_cond = self.context.append_basic_block(func, "iter_loop_cond");
            let iter_loop_body = self.context.append_basic_block(func, "iter_loop_body");
            let iter_loop_end = self.context.append_basic_block(func, "iter_loop_end");
            let assign_loop_cond = self.context.append_basic_block(func, "assign_loop_cond");
            let assign_loop_body = self.context.append_basic_block(func, "assign_loop_body");
            let assign_loop_end = self.context.append_basic_block(func, "assign_loop_end");
            let update_loop_cond = self.context.append_basic_block(func, "update_loop_cond");
            let update_loop_body = self.context.append_basic_block(func, "update_loop_body");
            let update_loop_end = self.context.append_basic_block(func, "update_loop_end");
            self.builder.position_at_end(entry);

            let data = func.get_nth_param(0).unwrap().into_pointer_value();
            let k = func.get_nth_param(1).unwrap().into_int_value();

            // Get data length
            let neg_16 = i64_type.const_int((-16i64) as u64, true);
            let len_ptr = unsafe {
                self.builder.build_gep(i8_type, data, &[neg_16], "len_ptr").map_err(|e| e.to_string())?
            };
            let len_ptr = self.builder.build_pointer_cast(len_ptr, ptr_type, "len_ptr_typed").map_err(|e| e.to_string())?;
            let n = self.builder.build_load(i64_type, len_ptr, "n").map_err(|e| e.to_string())?.into_int_value();

            // Allocate centroids array (k floats)
            let k_times_8 = self.builder.build_int_mul(k, i64_type.const_int(8, false), "k_times_8").map_err(|e| e.to_string())?;
            let centroids = self.builder.build_call(malloc, &[k_times_8.into()], "centroids")
                .map_err(|e| e.to_string())?.try_as_basic_value();
            let centroids = match centroids {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("malloc returned void".to_string()),
            };

            // Allocate assignments array (header + n ints)
            let header_size = i64_type.const_int(16, false);
            let n_times_8 = self.builder.build_int_mul(n, i64_type.const_int(8, false), "n_times_8").map_err(|e| e.to_string())?;
            let assign_total = self.builder.build_int_add(header_size, n_times_8, "assign_total").map_err(|e| e.to_string())?;
            let assign_base = self.builder.build_call(malloc, &[assign_total.into()], "assign_base")
                .map_err(|e| e.to_string())?.try_as_basic_value();
            let assign_base = match assign_base {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("malloc returned void".to_string()),
            };

            // Store length in assignments header
            self.builder.build_store(assign_base, n).map_err(|e| e.to_string())?;
            let cap_ptr = unsafe {
                self.builder.build_gep(i8_type, assign_base, &[i64_type.const_int(8, false)], "cap_ptr").map_err(|e| e.to_string())?
            };
            let cap_ptr = self.builder.build_pointer_cast(cap_ptr, ptr_type, "cap_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(cap_ptr, n).map_err(|e| e.to_string())?;
            let assignments = unsafe {
                self.builder.build_gep(i8_type, assign_base, &[header_size], "assignments").map_err(|e| e.to_string())?
            };

            // Initialize centroids with first k data points
            let counter = self.builder.build_alloca(i64_type, "counter").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, i64_type.const_int(0, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(init_loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(init_loop_cond);
            let i = self.builder.build_load(i64_type, counter, "i").map_err(|e| e.to_string())?.into_int_value();
            let cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i, k, "cond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cond, init_loop_body, init_loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(init_loop_body);
            let elem_size = i64_type.const_int(8, false);
            let offset = self.builder.build_int_mul(i, elem_size, "offset").map_err(|e| e.to_string())?;
            let data_ptr = unsafe {
                self.builder.build_gep(i8_type, data, &[offset], "data_ptr").map_err(|e| e.to_string())?
            };
            let data_ptr = self.builder.build_pointer_cast(data_ptr, ptr_type, "data_ptr_typed").map_err(|e| e.to_string())?;
            let val = self.builder.build_load(f64_type, data_ptr, "val").map_err(|e| e.to_string())?;
            let cent_ptr = unsafe {
                self.builder.build_gep(i8_type, centroids, &[offset], "cent_ptr").map_err(|e| e.to_string())?
            };
            let cent_ptr = self.builder.build_pointer_cast(cent_ptr, ptr_type, "cent_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(cent_ptr, val).map_err(|e| e.to_string())?;
            let next_i = self.builder.build_int_add(i, i64_type.const_int(1, false), "next_i").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_i).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(init_loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(init_loop_end);

            // Main iteration loop (10 iterations for simplicity)
            let iter_counter = self.builder.build_alloca(i64_type, "iter_counter").map_err(|e| e.to_string())?;
            self.builder.build_store(iter_counter, i64_type.const_int(0, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(iter_loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(iter_loop_cond);
            let iter = self.builder.build_load(i64_type, iter_counter, "iter").map_err(|e| e.to_string())?.into_int_value();
            let max_iter = i64_type.const_int(10, false);
            let iter_cond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, iter, max_iter, "iter_cond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(iter_cond, iter_loop_body, iter_loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(iter_loop_body);

            // Assignment step: assign each point to nearest centroid
            self.builder.build_store(counter, i64_type.const_int(0, false)).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(assign_loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(assign_loop_cond);
            let ai = self.builder.build_load(i64_type, counter, "ai").map_err(|e| e.to_string())?.into_int_value();
            let acond = self.builder.build_int_compare(inkwell::IntPredicate::SLT, ai, n, "acond").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(acond, assign_loop_body, assign_loop_end).map_err(|e| e.to_string())?;

            self.builder.position_at_end(assign_loop_body);
            // Get data point
            let a_offset = self.builder.build_int_mul(ai, elem_size, "a_offset").map_err(|e| e.to_string())?;
            let dp_ptr = unsafe {
                self.builder.build_gep(i8_type, data, &[a_offset], "dp_ptr").map_err(|e| e.to_string())?
            };
            let dp_ptr = self.builder.build_pointer_cast(dp_ptr, ptr_type, "dp_ptr_typed").map_err(|e| e.to_string())?;
            let point = self.builder.build_load(f64_type, dp_ptr, "point").map_err(|e| e.to_string())?.into_float_value();

            // Find nearest centroid (simplified: just use centroid 0 or 1 based on distance)
            // For simplicity, compare to first centroid
            let c0_ptr = self.builder.build_pointer_cast(centroids, ptr_type, "c0_ptr").map_err(|e| e.to_string())?;
            let c0 = self.builder.build_load(f64_type, c0_ptr, "c0").map_err(|e| e.to_string())?.into_float_value();
            let diff0 = self.builder.build_float_sub(point, c0, "diff0").map_err(|e| e.to_string())?;
            let fabs = self.module.get_function("fabs").ok_or("fabs not declared")?;
            let dist0 = self.builder.build_call(fabs, &[diff0.into()], "dist0")
                .map_err(|e| e.to_string())?.try_as_basic_value();
            let dist0 = match dist0 {
                ValueKind::Basic(v) => v.into_float_value(),
                _ => return Err("fabs returned void".to_string()),
            };

            // Simple assignment: cluster 0 if closer to first centroid, else cluster (k-1)
            let c_last_offset = self.builder.build_int_mul(
                self.builder.build_int_sub(k, i64_type.const_int(1, false), "k_minus_1").map_err(|e| e.to_string())?,
                elem_size, "c_last_offset"
            ).map_err(|e| e.to_string())?;
            let c_last_ptr = unsafe {
                self.builder.build_gep(i8_type, centroids, &[c_last_offset], "c_last_ptr").map_err(|e| e.to_string())?
            };
            let c_last_ptr = self.builder.build_pointer_cast(c_last_ptr, ptr_type, "c_last_ptr_typed").map_err(|e| e.to_string())?;
            let c_last = self.builder.build_load(f64_type, c_last_ptr, "c_last").map_err(|e| e.to_string())?.into_float_value();
            let diff_last = self.builder.build_float_sub(point, c_last, "diff_last").map_err(|e| e.to_string())?;
            let dist_last = self.builder.build_call(fabs, &[diff_last.into()], "dist_last")
                .map_err(|e| e.to_string())?.try_as_basic_value();
            let dist_last = match dist_last {
                ValueKind::Basic(v) => v.into_float_value(),
                _ => return Err("fabs returned void".to_string()),
            };

            let is_closer_to_first = self.builder.build_float_compare(
                inkwell::FloatPredicate::OLT, dist0, dist_last, "is_closer"
            ).map_err(|e| e.to_string())?;
            let cluster = self.builder.build_select(
                is_closer_to_first,
                i64_type.const_int(0, false),
                self.builder.build_int_sub(k, i64_type.const_int(1, false), "k_1").map_err(|e| e.to_string())?,
                "cluster"
            ).map_err(|e| e.to_string())?;

            // Store assignment
            let assign_ptr = unsafe {
                self.builder.build_gep(i8_type, assignments, &[a_offset], "assign_ptr").map_err(|e| e.to_string())?
            };
            let assign_ptr = self.builder.build_pointer_cast(assign_ptr, ptr_type, "assign_ptr_typed").map_err(|e| e.to_string())?;
            self.builder.build_store(assign_ptr, cluster).map_err(|e| e.to_string())?;

            let next_ai = self.builder.build_int_add(ai, i64_type.const_int(1, false), "next_ai").map_err(|e| e.to_string())?;
            self.builder.build_store(counter, next_ai).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(assign_loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(assign_loop_end);
            // Skip update step for simplicity (would need per-cluster sum/count)

            let next_iter = self.builder.build_int_add(iter, i64_type.const_int(1, false), "next_iter").map_err(|e| e.to_string())?;
            self.builder.build_store(iter_counter, next_iter).map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(iter_loop_cond).map_err(|e| e.to_string())?;

            self.builder.position_at_end(iter_loop_end);
            self.builder.build_return(Some(&assignments)).map_err(|e| e.to_string())?;

            for name in &["kMeans", "cluster"] {
                self.builtin_functions.insert(name.to_string(), func);
                self.builtin_return_types.insert(name.to_string(), Type::List(Box::new(Type::Int)));
            }
        }

        // ==================== PLOTTING FUNCTION ====================

        // --- plot(data: ptr, against: ptr (or null), chart_type: i64, title: ptr, filename: ptr) -> i1 ---
        // Generates an HTML file with embedded Plotly.js chart
        {
            // Get C functions by name
            let plot_fopen = self.module.get_function("fopen").ok_or("fopen not declared")?;
            let plot_fclose = self.module.get_function("fclose").ok_or("fclose not declared")?;
            let plot_fprintf = self.module.get_function("fprintf").ok_or("fprintf not declared")?;

            let fn_type = bool_type.fn_type(&[
                ptr_type.into(),  // data array
                ptr_type.into(),  // against array (or null for single data)
                i64_type.into(),  // chart_type: 0=line, 1=bar, 2=scatter, 3=histogram
                ptr_type.into(),  // title (or null)
                ptr_type.into(),  // output filename
            ], false);
            let func = self.module.add_function("englang_plot", fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            let write_header = self.context.append_basic_block(func, "write_header");
            let write_data = self.context.append_basic_block(func, "write_data");
            let data_loop_cond = self.context.append_basic_block(func, "data_loop_cond");
            let data_loop_body = self.context.append_basic_block(func, "data_loop_body");
            let data_loop_end = self.context.append_basic_block(func, "data_loop_end");
            let write_footer = self.context.append_basic_block(func, "write_footer");
            let success_bb = self.context.append_basic_block(func, "success");
            let fail_bb = self.context.append_basic_block(func, "fail");

            self.builder.position_at_end(entry);

            let data = func.get_nth_param(0).unwrap().into_pointer_value();
            let _against = func.get_nth_param(1).unwrap().into_pointer_value();
            let chart_type_val = func.get_nth_param(2).unwrap().into_int_value();
            let _title = func.get_nth_param(3).unwrap().into_pointer_value();
            let filename = func.get_nth_param(4).unwrap().into_pointer_value();

            // Get data array length (stored at offset -16 from data pointer)
            let len_offset = i64_type.const_int((-16i64) as u64, true);
            let len_ptr = unsafe {
                self.builder.build_gep(i8_type, data, &[len_offset], "len_ptr")
                    .map_err(|e| e.to_string())?
            };
            let len_ptr_i64 = self.builder
                .build_pointer_cast(len_ptr, self.context.ptr_type(inkwell::AddressSpace::default()), "len_ptr_i64")
                .map_err(|e| e.to_string())?;
            let data_len = self.builder
                .build_load(i64_type, len_ptr_i64, "data_len")
                .map_err(|e| e.to_string())?
                .into_int_value();

            // Open file for writing
            let mode_str = self.builder.build_global_string_ptr("w", "write_mode").map_err(|e| e.to_string())?;
            let file = self.builder
                .build_call(plot_fopen, &[filename.into(), mode_str.as_pointer_value().into()], "file")
                .map_err(|e| e.to_string())?
                .try_as_basic_value();
            let file_ptr = match file {
                ValueKind::Basic(v) => v.into_pointer_value(),
                _ => return Err("fopen returned void".to_string()),
            };

            let is_null = self.builder.build_is_null(file_ptr, "is_null").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(is_null, fail_bb, write_header).map_err(|e| e.to_string())?;

            // write_header: Write HTML header with Plotly.js CDN
            self.builder.position_at_end(write_header);
            let html_header = self.builder.build_global_string_ptr(
                "<!DOCTYPE html>\n<html><head>\n<script src=\"https://cdn.plot.ly/plotly-2.24.1.min.js\"></script>\n</head><body>\n<div id=\"chart\"></div>\n<script>\nvar yData = [",
                "html_header"
            ).map_err(|e| e.to_string())?;
            let fmt_s = self.builder.build_global_string_ptr("%s", "fmt_s").map_err(|e| e.to_string())?;
            self.builder.build_call(plot_fprintf, &[file_ptr.into(), fmt_s.as_pointer_value().into(), html_header.as_pointer_value().into()], "")
                .map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(write_data).map_err(|e| e.to_string())?;

            // write_data: Initialize loop
            self.builder.position_at_end(write_data);
            self.builder.build_unconditional_branch(data_loop_cond).map_err(|e| e.to_string())?;

            // data_loop_cond: Check if i < len
            self.builder.position_at_end(data_loop_cond);
            let i_phi = self.builder.build_phi(i64_type, "i").map_err(|e| e.to_string())?;
            i_phi.add_incoming(&[(&i64_type.const_int(0, false), write_data)]);
            let i_val = i_phi.as_basic_value().into_int_value();
            let cmp = self.builder.build_int_compare(inkwell::IntPredicate::SLT, i_val, data_len, "cmp").map_err(|e| e.to_string())?;
            self.builder.build_conditional_branch(cmp, data_loop_body, data_loop_end).map_err(|e| e.to_string())?;

            // data_loop_body: Write each element
            self.builder.position_at_end(data_loop_body);
            // Get element: data[i]
            let elem_offset = self.builder.build_int_mul(i_val, i64_type.const_int(8, false), "elem_offset").map_err(|e| e.to_string())?;
            let elem_ptr = unsafe {
                self.builder.build_gep(i8_type, data, &[elem_offset], "elem_ptr").map_err(|e| e.to_string())?
            };
            let elem_ptr_f64 = self.builder
                .build_pointer_cast(elem_ptr, self.context.ptr_type(inkwell::AddressSpace::default()), "elem_ptr_f64")
                .map_err(|e| e.to_string())?;
            let elem = self.builder.build_load(f64_type, elem_ptr_f64, "elem").map_err(|e| e.to_string())?.into_float_value();

            // Write comma if not first element
            let is_first = self.builder.build_int_compare(inkwell::IntPredicate::EQ, i_val, i64_type.const_int(0, false), "is_first").map_err(|e| e.to_string())?;
            let write_comma_bb = self.context.append_basic_block(func, "write_comma");
            let write_num_bb = self.context.append_basic_block(func, "write_num");
            self.builder.build_conditional_branch(is_first, write_num_bb, write_comma_bb).map_err(|e| e.to_string())?;

            self.builder.position_at_end(write_comma_bb);
            let comma_fmt = self.builder.build_global_string_ptr(",%.6g", "comma_fmt").map_err(|e| e.to_string())?;
            self.builder.build_call(plot_fprintf, &[file_ptr.into(), comma_fmt.as_pointer_value().into(), elem.into()], "").map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(write_num_bb).map_err(|e| e.to_string())?;

            // Merge point - but first element case
            let after_comma_bb = self.context.append_basic_block(func, "after_comma");
            self.builder.position_at_end(write_num_bb);
            let is_first2 = self.builder.build_int_compare(inkwell::IntPredicate::EQ, i_val, i64_type.const_int(0, false), "is_first2").map_err(|e| e.to_string())?;
            let write_first_bb = self.context.append_basic_block(func, "write_first");
            self.builder.build_conditional_branch(is_first2, write_first_bb, after_comma_bb).map_err(|e| e.to_string())?;

            self.builder.position_at_end(write_first_bb);
            let num_fmt = self.builder.build_global_string_ptr("%.6g", "num_fmt").map_err(|e| e.to_string())?;
            self.builder.build_call(plot_fprintf, &[file_ptr.into(), num_fmt.as_pointer_value().into(), elem.into()], "").map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(after_comma_bb).map_err(|e| e.to_string())?;

            self.builder.position_at_end(after_comma_bb);
            let i_next = self.builder.build_int_add(i_val, i64_type.const_int(1, false), "i_next").map_err(|e| e.to_string())?;
            i_phi.add_incoming(&[(&i_next, after_comma_bb)]);
            self.builder.build_unconditional_branch(data_loop_cond).map_err(|e| e.to_string())?;

            // data_loop_end: Write footer
            self.builder.position_at_end(data_loop_end);
            self.builder.build_unconditional_branch(write_footer).map_err(|e| e.to_string())?;

            // write_footer: Determine chart type and write appropriate Plotly config
            self.builder.position_at_end(write_footer);

            // Check chart type and write appropriate trace config
            // 0=line, 1=bar, 2=scatter, 3=histogram
            let is_line = self.builder.build_int_compare(inkwell::IntPredicate::EQ, chart_type_val, i64_type.const_int(0, false), "is_line").map_err(|e| e.to_string())?;
            let is_bar = self.builder.build_int_compare(inkwell::IntPredicate::EQ, chart_type_val, i64_type.const_int(1, false), "is_bar").map_err(|e| e.to_string())?;
            let is_scatter = self.builder.build_int_compare(inkwell::IntPredicate::EQ, chart_type_val, i64_type.const_int(2, false), "is_scatter").map_err(|e| e.to_string())?;

            // For simplicity, use a single trace format for all types - mode/type varies
            let line_footer = self.builder.build_global_string_ptr(
                "];\nPlotly.newPlot('chart', [{y: yData, type: 'scatter', mode: 'lines'}], {title: 'Chart'});\n</script>\n</body></html>",
                "line_footer"
            ).map_err(|e| e.to_string())?;
            let bar_footer = self.builder.build_global_string_ptr(
                "];\nPlotly.newPlot('chart', [{y: yData, type: 'bar'}], {title: 'Chart'});\n</script>\n</body></html>",
                "bar_footer"
            ).map_err(|e| e.to_string())?;
            let scatter_footer = self.builder.build_global_string_ptr(
                "];\nPlotly.newPlot('chart', [{y: yData, type: 'scatter', mode: 'markers'}], {title: 'Chart'});\n</script>\n</body></html>",
                "scatter_footer"
            ).map_err(|e| e.to_string())?;
            let histogram_footer = self.builder.build_global_string_ptr(
                "];\nPlotly.newPlot('chart', [{x: yData, type: 'histogram'}], {title: 'Chart'});\n</script>\n</body></html>",
                "histogram_footer"
            ).map_err(|e| e.to_string())?;

            // Select the right footer based on chart type
            let footer1 = self.builder.build_select(is_line, line_footer.as_pointer_value(), bar_footer.as_pointer_value(), "footer1").map_err(|e| e.to_string())?;
            let footer2 = self.builder.build_select(is_bar, bar_footer.as_pointer_value(), footer1.into_pointer_value(), "footer2").map_err(|e| e.to_string())?;
            let footer3 = self.builder.build_select(is_scatter, scatter_footer.as_pointer_value(), footer2.into_pointer_value(), "footer3").map_err(|e| e.to_string())?;

            // Default to histogram if not line/bar/scatter
            let is_histogram = self.builder.build_int_compare(inkwell::IntPredicate::EQ, chart_type_val, i64_type.const_int(3, false), "is_histogram").map_err(|e| e.to_string())?;
            let final_footer = self.builder.build_select(is_histogram, histogram_footer.as_pointer_value(), footer3.into_pointer_value(), "final_footer").map_err(|e| e.to_string())?;

            self.builder.build_call(plot_fprintf, &[file_ptr.into(), fmt_s.as_pointer_value().into(), final_footer.into_pointer_value().into()], "")
                .map_err(|e| e.to_string())?;

            // Close file
            self.builder.build_call(plot_fclose, &[file_ptr.into()], "").map_err(|e| e.to_string())?;
            self.builder.build_unconditional_branch(success_bb).map_err(|e| e.to_string())?;

            // success: return true
            self.builder.position_at_end(success_bb);
            self.builder.build_return(Some(&bool_type.const_int(1, false))).map_err(|e| e.to_string())?;

            // fail: return false
            self.builder.position_at_end(fail_bb);
            self.builder.build_return(Some(&bool_type.const_int(0, false))).map_err(|e| e.to_string())?;

            // Note: We don't register this as a builtin since it's called via compile_plot
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
                else_ifs,
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

                let merge_bb = self.context.append_basic_block(function, "merge");

                // Create blocks for the then branch and the first else-if/else
                let then_bb = self.context.append_basic_block(function, "then");
                let mut next_bb = if !else_ifs.is_empty() || else_block.is_some() {
                    self.context.append_basic_block(function, "elif_or_else")
                } else {
                    merge_bb
                };

                self.builder
                    .build_conditional_branch(cond_bool, then_bb, next_bb)
                    .map_err(|e| e.to_string())?;

                // Compile then block
                self.builder.position_at_end(then_bb);
                for stmt in then_block {
                    self.compile_statement(stmt)?;
                }
                // Only add branch if block doesn't already have a terminator (e.g., from break/return)
                if !self.current_block_has_terminator() {
                    self.builder
                        .build_unconditional_branch(merge_bb)
                        .map_err(|e| e.to_string())?;
                }

                // Compile else-if chains
                for (i, (elif_cond, elif_block)) in else_ifs.iter().enumerate() {
                    self.builder.position_at_end(next_bb);

                    let elif_cond_value = self.compile_expression(elif_cond)?;
                    let elif_cond_bool = match elif_cond_value {
                        BasicValueEnum::IntValue(v) => v,
                        _ => return Err("Condition must be boolean".to_string()),
                    };

                    let elif_then_bb = self.context.append_basic_block(function, "elif_then");
                    next_bb = if i + 1 < else_ifs.len() || else_block.is_some() {
                        self.context.append_basic_block(function, "elif_or_else")
                    } else {
                        merge_bb
                    };

                    self.builder
                        .build_conditional_branch(elif_cond_bool, elif_then_bb, next_bb)
                        .map_err(|e| e.to_string())?;

                    self.builder.position_at_end(elif_then_bb);
                    for stmt in elif_block {
                        self.compile_statement(stmt)?;
                    }
                    if !self.current_block_has_terminator() {
                        self.builder
                            .build_unconditional_branch(merge_bb)
                            .map_err(|e| e.to_string())?;
                    }
                }

                // Compile else block
                if let Some(else_stmts) = else_block {
                    self.builder.position_at_end(next_bb);
                    for stmt in else_stmts {
                        self.compile_statement(stmt)?;
                    }
                    if !self.current_block_has_terminator() {
                        self.builder
                            .build_unconditional_branch(merge_bb)
                            .map_err(|e| e.to_string())?;
                    }
                }

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

                // Push loop context for break/continue
                self.loop_break_stack.push(after_bb);
                self.loop_continue_stack.push(cond_bb);

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
                if !self.current_block_has_terminator() {
                    self.builder
                        .build_unconditional_branch(cond_bb)
                        .map_err(|e| e.to_string())?;
                }

                // Pop loop context
                self.loop_break_stack.pop();
                self.loop_continue_stack.pop();

                self.builder.position_at_end(after_bb);
            }

            Statement::For {
                variable,
                start,
                end,
                body,
            } => {
                let function = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();

                // Compile start and end values
                let start_val = self.compile_expression(start)?;
                let end_val = self.compile_expression(end)?;

                let start_int = match start_val {
                    BasicValueEnum::IntValue(v) => v,
                    _ => return Err("For loop start must be an integer".to_string()),
                };
                let end_int = match end_val {
                    BasicValueEnum::IntValue(v) => v,
                    _ => return Err("For loop end must be an integer".to_string()),
                };

                // Allocate loop variable
                let i64_type = self.context.i64_type();
                let var_ptr = self
                    .builder
                    .build_alloca(i64_type, variable)
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(var_ptr, start_int)
                    .map_err(|e| e.to_string())?;
                self.variables
                    .insert(variable.clone(), (var_ptr, Type::Int));

                let cond_bb = self.context.append_basic_block(function, "for_cond");
                let body_bb = self.context.append_basic_block(function, "for_body");
                let incr_bb = self.context.append_basic_block(function, "for_incr");
                let after_bb = self.context.append_basic_block(function, "for_after");

                // Push loop context for break/continue
                self.loop_break_stack.push(after_bb);
                self.loop_continue_stack.push(incr_bb);

                self.builder
                    .build_unconditional_branch(cond_bb)
                    .map_err(|e| e.to_string())?;

                // Condition: var <= end
                self.builder.position_at_end(cond_bb);
                let current_val = self
                    .builder
                    .build_load(i64_type, var_ptr, "load_i")
                    .map_err(|e| e.to_string())?
                    .into_int_value();
                let cond = self
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::SLE,
                        current_val,
                        end_int,
                        "for_cond",
                    )
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_conditional_branch(cond, body_bb, after_bb)
                    .map_err(|e| e.to_string())?;

                // Body
                self.builder.position_at_end(body_bb);
                for stmt in body {
                    self.compile_statement(stmt)?;
                }
                if !self.current_block_has_terminator() {
                    self.builder
                        .build_unconditional_branch(incr_bb)
                        .map_err(|e| e.to_string())?;
                }

                // Increment
                self.builder.position_at_end(incr_bb);
                let current_val = self
                    .builder
                    .build_load(i64_type, var_ptr, "load_i")
                    .map_err(|e| e.to_string())?
                    .into_int_value();
                let one = i64_type.const_int(1, false);
                let next_val = self
                    .builder
                    .build_int_add(current_val, one, "inc_i")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(var_ptr, next_val)
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_unconditional_branch(cond_bb)
                    .map_err(|e| e.to_string())?;

                // Pop loop context
                self.loop_break_stack.pop();
                self.loop_continue_stack.pop();

                // After
                self.builder.position_at_end(after_bb);

                // Remove the loop variable from scope
                self.variables.remove(variable);
            }

            Statement::Break => {
                if let Some(break_bb) = self.loop_break_stack.last() {
                    self.builder
                        .build_unconditional_branch(*break_bb)
                        .map_err(|e| e.to_string())?;
                } else {
                    return Err("Break outside of loop".to_string());
                }
            }

            Statement::Continue => {
                if let Some(continue_bb) = self.loop_continue_stack.last() {
                    self.builder
                        .build_unconditional_branch(*continue_bb)
                        .map_err(|e| e.to_string())?;
                } else {
                    return Err("Continue outside of loop".to_string());
                }
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

            Statement::Plot {
                data,
                against,
                chart_type,
                title,
                output_file,
            } => {
                // Compile the plot statement - generates an HTML file with Plotly.js
                self.compile_plot(data, against.as_ref(), chart_type, title.as_deref(), output_file)?;
            }
        }

        Ok(())
    }

    /// Compile a plot statement - generates an HTML file with embedded Plotly.js chart
    fn compile_plot(
        &mut self,
        data: &Expr,
        against: Option<&Expr>,
        chart_type: &ChartType,
        title: Option<&str>,
        output_file: &str,
    ) -> Result<(), String> {
        // Get the plot function
        let plot_fn = self
            .module
            .get_function("englang_plot")
            .ok_or("englang_plot not declared")?;

        // Compile the data expression (should return array pointer)
        let data_val = self.compile_expression(data)?;
        let data_ptr = data_val.into_pointer_value();

        // Compile the against expression if present, otherwise null
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let against_ptr = if let Some(against_expr) = against {
            self.compile_expression(against_expr)?.into_pointer_value()
        } else {
            ptr_type.const_null()
        };

        // Convert chart type to integer: 0=line, 1=bar, 2=scatter, 3=histogram
        let chart_type_int = match chart_type {
            ChartType::Line => 0,
            ChartType::Bar => 1,
            ChartType::Scatter => 2,
            ChartType::Histogram => 3,
        };
        let chart_type_val = self.context.i64_type().const_int(chart_type_int, false);

        // Create title string (or null)
        let title_ptr = if let Some(t) = title {
            self.builder
                .build_global_string_ptr(t, "plot_title")
                .map_err(|e| e.to_string())?
                .as_pointer_value()
        } else {
            ptr_type.const_null()
        };

        // Create output filename string
        let filename_ptr = self
            .builder
            .build_global_string_ptr(output_file, "plot_filename")
            .map_err(|e| e.to_string())?
            .as_pointer_value();

        // Call the plot function
        self.builder
            .build_call(
                plot_fn,
                &[
                    data_ptr.into(),
                    against_ptr.into(),
                    chart_type_val.into(),
                    title_ptr.into(),
                    filename_ptr.into(),
                ],
                "plot_result",
            )
            .map_err(|e| e.to_string())?;

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

            Expr::ListLiteral(elements) => {
                let i64_type = self.context.i64_type();
                let i8_type = self.context.i8_type();
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                let malloc = self.malloc_fn.ok_or("malloc not declared")?;

                let len = elements.len() as u64;
                let header_size = 16u64; // 2 * i64 for length and capacity
                let elem_size = 8u64; // 8 bytes per element
                let total_size = header_size + (len * elem_size);

                // Allocate memory
                let size_val = i64_type.const_int(total_size, false);
                let alloc_result = self
                    .builder
                    .build_call(malloc, &[size_val.into()], "list_alloc")
                    .map_err(|e| e.to_string())?
                    .try_as_basic_value();
                let base_ptr = match alloc_result {
                    ValueKind::Basic(v) => v.into_pointer_value(),
                    _ => return Err("malloc returned void".to_string()),
                };

                // Store length at offset 0
                let len_val = i64_type.const_int(len, false);
                self.builder
                    .build_store(base_ptr, len_val)
                    .map_err(|e| e.to_string())?;

                // Store capacity at offset 8
                let cap_ptr = unsafe {
                    self.builder
                        .build_gep(i8_type, base_ptr, &[i64_type.const_int(8, false)], "cap_ptr")
                        .map_err(|e| e.to_string())?
                };
                let cap_ptr_typed = self
                    .builder
                    .build_pointer_cast(cap_ptr, ptr_type, "cap_ptr_typed")
                    .map_err(|e| e.to_string())?;
                self.builder
                    .build_store(cap_ptr_typed, len_val)
                    .map_err(|e| e.to_string())?;

                // Data starts at offset 16
                let data_ptr = unsafe {
                    self.builder
                        .build_gep(i8_type, base_ptr, &[i64_type.const_int(header_size, false)], "data_ptr")
                        .map_err(|e| e.to_string())?
                };

                // Store each element
                for (i, elem) in elements.iter().enumerate() {
                    let val = self.compile_expression(elem)?;
                    let offset = i64_type.const_int((i as u64) * elem_size, false);
                    let elem_ptr = unsafe {
                        self.builder
                            .build_gep(i8_type, data_ptr, &[offset], "elem_ptr")
                            .map_err(|e| e.to_string())?
                    };

                    // Cast to appropriate type and store
                    match val {
                        BasicValueEnum::IntValue(_) | BasicValueEnum::FloatValue(_) => {
                            let typed_ptr = self
                                .builder
                                .build_pointer_cast(elem_ptr, ptr_type, "typed_elem_ptr")
                                .map_err(|e| e.to_string())?;
                            self.builder
                                .build_store(typed_ptr, val)
                                .map_err(|e| e.to_string())?;
                        }
                        BasicValueEnum::PointerValue(pv) => {
                            let typed_ptr = self
                                .builder
                                .build_pointer_cast(elem_ptr, ptr_type, "typed_elem_ptr")
                                .map_err(|e| e.to_string())?;
                            self.builder
                                .build_store(typed_ptr, pv)
                                .map_err(|e| e.to_string())?;
                        }
                        _ => return Err("Unsupported element type in list literal".to_string()),
                    }
                }

                Ok(data_ptr.into())
            }

            Expr::Index { collection, index } => {
                let i64_type = self.context.i64_type();
                let i8_type = self.context.i8_type();
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());

                let coll_ptr = self.compile_expression(collection)?;
                let idx_val = self.compile_expression(index)?;

                let coll_ptr = match coll_ptr {
                    BasicValueEnum::PointerValue(pv) => pv,
                    _ => return Err("Collection must be a pointer".to_string()),
                };

                let idx = match idx_val {
                    BasicValueEnum::IntValue(iv) => iv,
                    _ => return Err("Index must be an integer".to_string()),
                };

                // Calculate element offset: idx * 8
                let elem_size = i64_type.const_int(8, false);
                let offset = self
                    .builder
                    .build_int_mul(idx, elem_size, "offset")
                    .map_err(|e| e.to_string())?;

                // Get element pointer
                let elem_ptr = unsafe {
                    self.builder
                        .build_gep(i8_type, coll_ptr, &[offset], "elem_ptr")
                        .map_err(|e| e.to_string())?
                };

                // Load the element (assume i64 for now, could be more sophisticated)
                let typed_ptr = self
                    .builder
                    .build_pointer_cast(elem_ptr, ptr_type, "typed_elem_ptr")
                    .map_err(|e| e.to_string())?;
                let val = self
                    .builder
                    .build_load(i64_type, typed_ptr, "elem_val")
                    .map_err(|e| e.to_string())?;

                Ok(val)
            }

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
            Type::List(_) => self.context.ptr_type(inkwell::AddressSpace::default()).into(),
            Type::Dict(_, _) => self.context.ptr_type(inkwell::AddressSpace::default()).into(),
            Type::Tuple(_) => self.context.ptr_type(inkwell::AddressSpace::default()).into(),
            Type::Set(_) => self.context.ptr_type(inkwell::AddressSpace::default()).into(),
            Type::Void => self.context.i64_type().into(), // shouldn't be used as value type
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
            Expr::ListLiteral(elements) => {
                // Infer element type from first element
                if let Some(first) = elements.first() {
                    Type::List(Box::new(self.infer_type(first)))
                } else {
                    Type::List(Box::new(Type::Int)) // default empty list to int
                }
            }
            Expr::Index { collection, .. } => {
                // Return element type of the list
                if let Type::List(elem_type) = self.infer_type(collection) {
                    *elem_type
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
