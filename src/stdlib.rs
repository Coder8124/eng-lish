use crate::ast::Type;

/// Describes a single builtin function in the standard library.
#[derive(Debug, Clone)]
pub struct BuiltinFunction {
    /// All names this function can be called by (English name first, then aliases)
    pub names: Vec<&'static str>,
    /// Parameter definitions: (param_name, param_type)
    pub parameters: Vec<(&'static str, Type)>,
    /// The return type of this function
    pub return_type: Type,
    /// If Some, this is the C function name to call directly.
    /// If None, a custom LLVM IR function must be built in codegen.
    pub c_function: Option<&'static str>,
    /// If true, Int arguments should be auto-promoted to Float at call sites.
    pub accepts_int_as_float: bool,
}

pub fn get_all_builtins() -> Vec<BuiltinFunction> {
    let mut builtins = Vec::new();
    builtins.extend(get_math_builtins());
    builtins.extend(get_string_builtins());
    builtins
}

pub fn get_math_builtins() -> Vec<BuiltinFunction> {
    vec![
        // f64 -> f64 functions
        BuiltinFunction {
            names: vec!["squareRoot", "sqrt"],
            parameters: vec![("x", Type::Float)],
            return_type: Type::Float,
            c_function: Some("sqrt"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["absoluteValue", "abs"],
            parameters: vec![("x", Type::Float)],
            return_type: Type::Float,
            c_function: Some("fabs"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["floor"],
            parameters: vec![("x", Type::Float)],
            return_type: Type::Float,
            c_function: Some("floor"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["ceiling", "ceil"],
            parameters: vec![("x", Type::Float)],
            return_type: Type::Float,
            c_function: Some("ceil"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["round"],
            parameters: vec![("x", Type::Float)],
            return_type: Type::Float,
            c_function: Some("round"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["sine", "sin"],
            parameters: vec![("x", Type::Float)],
            return_type: Type::Float,
            c_function: Some("sin"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["cosine", "cos"],
            parameters: vec![("x", Type::Float)],
            return_type: Type::Float,
            c_function: Some("cos"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["tangent", "tan"],
            parameters: vec![("x", Type::Float)],
            return_type: Type::Float,
            c_function: Some("tan"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["arcSine", "asin"],
            parameters: vec![("x", Type::Float)],
            return_type: Type::Float,
            c_function: Some("asin"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["arcCosine", "acos"],
            parameters: vec![("x", Type::Float)],
            return_type: Type::Float,
            c_function: Some("acos"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["arcTangent", "atan"],
            parameters: vec![("x", Type::Float)],
            return_type: Type::Float,
            c_function: Some("atan"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["naturalLog", "ln"],
            parameters: vec![("x", Type::Float)],
            return_type: Type::Float,
            c_function: Some("log"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["logarithm", "log"],
            parameters: vec![("x", Type::Float)],
            return_type: Type::Float,
            c_function: Some("log10"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["exponential", "exp"],
            parameters: vec![("x", Type::Float)],
            return_type: Type::Float,
            c_function: Some("exp"),
            accepts_int_as_float: true,
        },
        // (f64, f64) -> f64 functions
        BuiltinFunction {
            names: vec!["power", "pow"],
            parameters: vec![("base", Type::Float), ("exponent", Type::Float)],
            return_type: Type::Float,
            c_function: Some("pow"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["minimum", "min"],
            parameters: vec![("a", Type::Float), ("b", Type::Float)],
            return_type: Type::Float,
            c_function: Some("fmin"),
            accepts_int_as_float: true,
        },
        BuiltinFunction {
            names: vec!["maximum", "max"],
            parameters: vec![("a", Type::Float), ("b", Type::Float)],
            return_type: Type::Float,
            c_function: Some("fmax"),
            accepts_int_as_float: true,
        },
    ]
}

pub fn get_string_builtins() -> Vec<BuiltinFunction> {
    vec![
        BuiltinFunction {
            names: vec!["lengthOf", "strlen"],
            parameters: vec![("s", Type::Text)],
            return_type: Type::Int,
            c_function: None, // custom implementation
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["combine", "concat"],
            parameters: vec![("a", Type::Text), ("b", Type::Text)],
            return_type: Type::Text,
            c_function: None,
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["characterAt", "charAt"],
            parameters: vec![("s", Type::Text), ("index", Type::Int)],
            return_type: Type::Text,
            c_function: None,
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["uppercase", "upper"],
            parameters: vec![("s", Type::Text)],
            return_type: Type::Text,
            c_function: None,
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["lowercase", "lower"],
            parameters: vec![("s", Type::Text)],
            return_type: Type::Text,
            c_function: None,
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["contains", "has"],
            parameters: vec![("haystack", Type::Text), ("needle", Type::Text)],
            return_type: Type::Bool,
            c_function: None,
            accepts_int_as_float: false,
        },
    ]
}
