use logos::Logos;

/// Token types for the eng-lish programming language
#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\r]+")]  // Skip whitespace
pub enum Token {
    // Keywords for variable declaration
    #[token("let", ignore(case))]
    Let,

    #[token("be", ignore(case))]
    Be,

    #[token("a", ignore(case))]
    A,

    #[token("an", ignore(case))]
    An,

    #[token("with", ignore(case))]
    With,

    #[token("value", ignore(case))]
    Value,

    // Type keywords (eng-lish types)
    #[token("text", ignore(case))]
    TypeText,

    #[token("standard number", ignore(case))]
    TypeInt,

    #[token("decimal", ignore(case))]
    TypeFloat,

    #[token("list", ignore(case))]
    TypeList,

    #[token("lock and key list", ignore(case))]
    TypeDict,

    #[token("fixed list", ignore(case))]
    TypeTuple,

    #[token("unique collection", ignore(case))]
    TypeSet,

    #[token("boolean", ignore(case))]
    TypeBool,

    // Boolean literals
    #[token("true", ignore(case))]
    True,

    #[token("false", ignore(case))]
    False,

    // Arithmetic operation keywords
    #[token("add", ignore(case))]
    Add,

    #[token("subtract", ignore(case))]
    Subtract,

    #[token("multiply", ignore(case))]
    Multiply,

    #[token("divide", ignore(case))]
    Divide,

    #[token("remainder", ignore(case))]
    Remainder,

    #[token("quotient", ignore(case))]
    Quotient,

    #[token("to", ignore(case))]
    To,

    #[token("by", ignore(case))]
    By,

    #[token("from", ignore(case))]
    From,

    // Comparison keywords
    #[token("same", ignore(case))]
    Same,

    #[token("equal", ignore(case))]
    Equal,

    #[token("is", ignore(case))]
    Is,

    #[token("not", ignore(case))]
    Not,

    #[token("greater", ignore(case))]
    Greater,

    #[token("less", ignore(case))]
    Less,

    #[token("than", ignore(case))]
    Than,

    // Control flow
    #[token("if", ignore(case))]
    If,

    #[token("then", ignore(case))]
    Then,

    #[token("else", ignore(case))]
    Else,

    #[token("otherwise", ignore(case))]
    Otherwise,

    #[token("while", ignore(case))]
    While,

    #[token("repeat", ignore(case))]
    Repeat,

    // Output
    #[token("output", ignore(case))]
    Output,

    // Type conversion (signifier)
    #[token("of", ignore(case))]
    Of,

    // Punctuation
    #[token(".")]
    Period,

    #[token(",")]
    Comma,

    #[token(":")]
    Colon,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    // Literals
    #[regex(r#""[^"]*""#, |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    StringLiteral(String),

    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().ok())]
    FloatLiteral(Option<f64>),

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    IntLiteral(Option<i64>),

    // Identifiers (variable/function names)
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string(), priority = 0)]
    Identifier(String),
}

/// Represents a token with its position in the source code
#[derive(Debug, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: std::ops::Range<usize>,
}

/// Lexer for the eng-lish language
pub struct Lexer<'source> {
    inner: logos::Lexer<'source, Token>,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            inner: Token::lexer(source),
        }
    }

    /// Tokenize the entire source and return a vector of spanned tokens
    pub fn tokenize(source: &str) -> Result<Vec<SpannedToken>, LexError> {
        let mut lexer = Token::lexer(source);
        let mut tokens = Vec::new();

        while let Some(result) = lexer.next() {
            match result {
                Ok(token) => {
                    tokens.push(SpannedToken {
                        token,
                        span: lexer.span(),
                    });
                }
                Err(_) => {
                    return Err(LexError {
                        span: lexer.span(),
                        message: format!("Unexpected token: '{}'", lexer.slice()),
                    });
                }
            }
        }

        Ok(tokens)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<SpannedToken, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|result| {
            match result {
                Ok(token) => Ok(SpannedToken {
                    token,
                    span: self.inner.span(),
                }),
                Err(_) => Err(LexError {
                    span: self.inner.span(),
                    message: format!("Unexpected token: '{}'", self.inner.slice()),
                }),
            }
        })
    }
}

#[derive(Debug, Clone)]
pub struct LexError {
    pub span: std::ops::Range<usize>,
    pub message: String,
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Lexical error at position {}-{}: {}",
               self.span.start, self.span.end, self.message)
    }
}

impl std::error::Error for LexError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_variable_declaration() {
        let source = "let x be a decimal with value 3.14.";
        let tokens = Lexer::tokenize(source).unwrap();

        assert!(matches!(tokens[0].token, Token::Let));
        assert!(matches!(&tokens[1].token, Token::Identifier(name) if name == "x"));
        assert!(matches!(tokens[2].token, Token::Be));
        assert!(matches!(tokens[3].token, Token::A));
        assert!(matches!(tokens[4].token, Token::TypeFloat));
        assert!(matches!(tokens[5].token, Token::With));
        assert!(matches!(tokens[6].token, Token::Value));
        assert!(matches!(&tokens[7].token, Token::FloatLiteral(Some(v)) if (*v - 3.14).abs() < 0.001));
        assert!(matches!(tokens[8].token, Token::Period));
    }

    #[test]
    fn test_output_statement() {
        let source = r#"output "Hello World"."#;
        let tokens = Lexer::tokenize(source).unwrap();

        assert!(matches!(tokens[0].token, Token::Output));
        assert!(matches!(&tokens[1].token, Token::StringLiteral(s) if s == "Hello World"));
        assert!(matches!(tokens[2].token, Token::Period));
    }

    #[test]
    fn test_arithmetic() {
        let source = "Add 5 to x.";
        let tokens = Lexer::tokenize(source).unwrap();

        assert!(matches!(tokens[0].token, Token::Add));
        assert!(matches!(&tokens[1].token, Token::IntLiteral(Some(5))));
        assert!(matches!(tokens[2].token, Token::To));
        assert!(matches!(&tokens[3].token, Token::Identifier(name) if name == "x"));
        assert!(matches!(tokens[4].token, Token::Period));
    }
}
