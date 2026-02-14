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
    builtins.extend(get_io_builtins());
    builtins.extend(get_utility_builtins());
    builtins.extend(get_array_builtins());
    builtins.extend(get_ml_builtins());
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

pub fn get_io_builtins() -> Vec<BuiltinFunction> {
    vec![
        BuiltinFunction {
            names: vec!["readLine", "readln", "input"],
            parameters: vec![],
            return_type: Type::Text,
            c_function: None, // custom implementation with fgets
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["readNumber", "readnum"],
            parameters: vec![],
            return_type: Type::Int,
            c_function: None, // custom implementation with scanf
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["readFile"],
            parameters: vec![("path", Type::Text)],
            return_type: Type::Text,
            c_function: None, // custom implementation with fopen/fread
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["writeFile"],
            parameters: vec![("path", Type::Text), ("content", Type::Text)],
            return_type: Type::Bool,
            c_function: None, // custom implementation with fopen/fwrite
            accepts_int_as_float: false,
        },
    ]
}

pub fn get_utility_builtins() -> Vec<BuiltinFunction> {
    vec![
        BuiltinFunction {
            names: vec!["textToNumber", "parseInt"],
            parameters: vec![("s", Type::Text)],
            return_type: Type::Int,
            c_function: Some("atoll"),
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["textToDecimal", "parseFloat"],
            parameters: vec![("s", Type::Text)],
            return_type: Type::Float,
            c_function: Some("atof"),
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["numberToText", "intToString"],
            parameters: vec![("n", Type::Int)],
            return_type: Type::Text,
            c_function: None, // custom implementation with snprintf
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["decimalToText", "floatToString"],
            parameters: vec![("f", Type::Float)],
            return_type: Type::Text,
            c_function: None, // custom implementation with snprintf
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["sleep", "wait", "delay"],
            parameters: vec![("ms", Type::Int)],
            return_type: Type::Void,
            c_function: None, // custom implementation with usleep
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["random"],
            parameters: vec![],
            return_type: Type::Float,
            c_function: None, // custom implementation with rand()
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["randomBetween", "randomInt"],
            parameters: vec![("min", Type::Int), ("max", Type::Int)],
            return_type: Type::Int,
            c_function: None, // custom implementation with rand()
            accepts_int_as_float: false,
        },
    ]
}

pub fn get_array_builtins() -> Vec<BuiltinFunction> {
    vec![
        // Array creation
        BuiltinFunction {
            names: vec!["zeros"],
            parameters: vec![("n", Type::Int)],
            return_type: Type::List(Box::new(Type::Float)),
            c_function: None,
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["ones"],
            parameters: vec![("n", Type::Int)],
            return_type: Type::List(Box::new(Type::Float)),
            c_function: None,
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["range"],
            parameters: vec![("start", Type::Int), ("end", Type::Int)],
            return_type: Type::List(Box::new(Type::Int)),
            c_function: None,
            accepts_int_as_float: false,
        },
        // Array aggregation
        BuiltinFunction {
            names: vec!["sum"],
            parameters: vec![("arr", Type::List(Box::new(Type::Float)))],
            return_type: Type::Float,
            c_function: None,
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["mean", "average"],
            parameters: vec![("arr", Type::List(Box::new(Type::Float)))],
            return_type: Type::Float,
            c_function: None,
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["arrayLength", "len"],
            parameters: vec![("arr", Type::List(Box::new(Type::Int)))],
            return_type: Type::Int,
            c_function: None,
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["arrayMin", "minOf"],
            parameters: vec![("arr", Type::List(Box::new(Type::Float)))],
            return_type: Type::Float,
            c_function: None,
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["arrayMax", "maxOf"],
            parameters: vec![("arr", Type::List(Box::new(Type::Float)))],
            return_type: Type::Float,
            c_function: None,
            accepts_int_as_float: false,
        },
        // Array manipulation
        BuiltinFunction {
            names: vec!["append", "push"],
            parameters: vec![("arr", Type::List(Box::new(Type::Int))), ("value", Type::Int)],
            return_type: Type::List(Box::new(Type::Int)),
            c_function: None,
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["reverse"],
            parameters: vec![("arr", Type::List(Box::new(Type::Int)))],
            return_type: Type::List(Box::new(Type::Int)),
            c_function: None,
            accepts_int_as_float: false,
        },
    ]
}

pub fn get_ml_builtins() -> Vec<BuiltinFunction> {
    vec![
        // Statistics
        BuiltinFunction {
            names: vec!["standardDeviation", "stdDev", "std"],
            parameters: vec![("arr", Type::List(Box::new(Type::Float)))],
            return_type: Type::Float,
            c_function: None,
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["variance", "var"],
            parameters: vec![("arr", Type::List(Box::new(Type::Float)))],
            return_type: Type::Float,
            c_function: None,
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["correlation", "corr"],
            parameters: vec![("x", Type::List(Box::new(Type::Float))), ("y", Type::List(Box::new(Type::Float)))],
            return_type: Type::Float,
            c_function: None,
            accepts_int_as_float: false,
        },
        // Linear Regression
        BuiltinFunction {
            names: vec!["fitLine", "linearRegression"],
            parameters: vec![("x", Type::List(Box::new(Type::Float))), ("y", Type::List(Box::new(Type::Float)))],
            return_type: Type::List(Box::new(Type::Float)), // [slope, intercept]
            c_function: None,
            accepts_int_as_float: false,
        },
        BuiltinFunction {
            names: vec!["predictLinear"],
            parameters: vec![("coefficients", Type::List(Box::new(Type::Float))), ("x", Type::Float)],
            return_type: Type::Float,
            c_function: None,
            accepts_int_as_float: false,
        },
        // Clustering
        BuiltinFunction {
            names: vec!["kMeans", "cluster"],
            parameters: vec![("data", Type::List(Box::new(Type::Float))), ("k", Type::Int)],
            return_type: Type::List(Box::new(Type::Int)), // cluster assignments
            c_function: None,
            accepts_int_as_float: false,
        },
    ]
}
