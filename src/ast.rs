/// Abstract Syntax Tree definitions for the eng-lish language

/// The type system for eng-lish
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Text,           // str in Python
    Int,            // standard number
    Float,          // decimal
    Bool,           // boolean
    List(Box<Type>),      // list of <type>
    Dict(Box<Type>, Box<Type>),  // lock and key list
    Tuple(Vec<Type>),     // fixed list
    Set(Box<Type>),       // unique collection
    Void,           // for functions that don't return
    Class(String),  // user-defined class type
}

/// A function/method parameter
#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
}

/// Function definition
#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub body: Vec<Statement>,
}

/// A class property
#[derive(Debug, Clone)]
pub struct Property {
    pub name: String,
    pub prop_type: Type,
}

/// Constructor definition
#[derive(Debug, Clone)]
pub struct Constructor {
    pub parameters: Vec<Parameter>,
    pub body: Vec<Statement>,
}

/// A method (function within a class)
#[derive(Debug, Clone)]
pub struct Method {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub body: Vec<Statement>,
}

/// Class definition
#[derive(Debug, Clone)]
pub struct ClassDef {
    pub name: String,
    pub parent: Option<String>,  // For inheritance
    pub properties: Vec<Property>,
    pub constructor: Option<Constructor>,
    pub methods: Vec<Method>,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Text => write!(f, "text"),
            Type::Int => write!(f, "standard number"),
            Type::Float => write!(f, "decimal"),
            Type::Bool => write!(f, "boolean"),
            Type::List(inner) => write!(f, "list of {}", inner),
            Type::Dict(key, value) => write!(f, "lock and key list of {} to {}", key, value),
            Type::Tuple(types) => {
                write!(f, "fixed list of (")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::Set(inner) => write!(f, "unique collection of {}", inner),
            Type::Void => write!(f, "nothing"),
            Type::Class(name) => write!(f, "{}", name),
        }
    }
}

/// Binary operators
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,   // %
    Quotient,    // //
    Equal,       // same / is equal to
    NotEqual,    // is not equal to
    Greater,     // is greater than
    Less,        // is less than
    GreaterEq,   // is greater than or equal to
    LessEq,      // is less than or equal to
    And,
    Or,
}

/// Unary operators
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,  // negative
    Not,     // not
}

/// Expressions in eng-lish
#[derive(Debug, Clone)]
pub enum Expr {
    /// Integer literal: 42
    IntLiteral(i64),

    /// Float literal: 3.14
    FloatLiteral(f64),

    /// String literal: "hello"
    StringLiteral(String),

    /// Boolean literal: true/false
    BoolLiteral(bool),

    /// Variable reference: varname
    Identifier(String),

    /// Binary operation: Add x to y
    BinaryOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    /// Unary operation: not x
    UnaryOp {
        op: UnaryOp,
        operand: Box<Expr>,
    },

    /// Type conversion (signifier): standard number of text
    TypeConversion {
        target_type: Type,
        expr: Box<Expr>,
    },

    /// List literal: list of 1, 2, 3
    ListLiteral(Vec<Expr>),

    /// Index access: element x of list
    Index {
        collection: Box<Expr>,
        index: Box<Expr>,
    },

    /// Function call: Call add with 5 and 10, or result of add with 5 and 10
    FunctionCall {
        name: String,
        arguments: Vec<Expr>,
    },

    /// Object instantiation: Person created with "Alice" and 30
    NewObject {
        class_name: String,
        arguments: Vec<Expr>,
    },

    /// Method call: Ask alice to greet
    MethodCall {
        object: Box<Expr>,
        method: String,
        arguments: Vec<Expr>,
    },

    /// Property access: the name of alice
    PropertyAccess {
        object: Box<Expr>,
        property: String,
    },
}

/// Statements in eng-lish
#[derive(Debug, Clone)]
pub enum Statement {
    /// Variable declaration: let x be a decimal with value 3.14.
    VariableDecl {
        name: String,
        var_type: Type,
        value: Expr,
    },

    /// Assignment: Set x to 5.
    Assignment {
        name: String,
        value: Expr,
    },

    /// Compound assignment: Add 5 to x.
    CompoundAssignment {
        name: String,
        op: BinaryOp,
        value: Expr,
    },

    /// Output statement: output "Hello World".
    Output(Expr),

    /// If statement: If x is greater than 5 then ... otherwise ...
    If {
        condition: Expr,
        then_block: Vec<Statement>,
        else_block: Option<Vec<Statement>>,
    },

    /// While loop: While x is less than 10, ...
    While {
        condition: Expr,
        body: Vec<Statement>,
    },

    /// Expression statement (for side effects)
    ExprStatement(Expr),

    /// Return statement: Give back x.
    Return(Option<Expr>),

    /// Property assignment: Set the name of alice to "Alice".
    PropertyAssignment {
        object: String,
        property: String,
        value: Expr,
    },
}

/// A complete eng-lish program
#[derive(Debug, Clone)]
pub struct Program {
    pub classes: Vec<ClassDef>,
    pub functions: Vec<FunctionDef>,
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            classes: Vec::new(),
            functions: Vec::new(),
            statements: Vec::new(),
        }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}
