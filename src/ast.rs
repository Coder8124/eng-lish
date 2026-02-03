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
}

/// A complete eng-lish program
#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}
