use crate::ast::*;
use crate::lexer::{Lexer, SpannedToken, Token};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Unexpected end of input")]
    UnexpectedEof,

    #[error("Unexpected token at position {position}: expected {expected}, found {found:?}")]
    UnexpectedToken {
        expected: String,
        found: Token,
        position: usize,
    },

    #[error("Invalid statement at position {0}: {1}")]
    InvalidStatement(usize, String),

    #[error("Expected type at position {0}")]
    ExpectedType(usize),

    #[error("Lexer error: {0}")]
    LexerError(String),
}

pub struct Parser {
    tokens: Vec<SpannedToken>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<SpannedToken>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(source: &str) -> Result<Program, ParseError> {
        let tokens = Lexer::tokenize(source).map_err(|e| ParseError::LexerError(e.message))?;
        let mut parser = Parser::new(tokens);
        parser.parse_program()
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }

        Ok(Program { statements })
    }

    // ========== Helper Methods ==========

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.current).map(|t| &t.token)
    }

    fn current_position(&self) -> usize {
        self.tokens
            .get(self.current)
            .map(|t| t.span.start)
            .unwrap_or(0)
    }

    fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens.get(self.current - 1).map(|t| &t.token)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn check(&self, token: &Token) -> bool {
        self.current() == Some(token)
    }

    fn match_token(&mut self, token: &Token) -> bool {
        if self.check(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, token: Token) -> Result<(), ParseError> {
        if self.check(&token) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: format!("{:?}", token),
                found: self.current().cloned().unwrap_or(Token::Period),
                position: self.current_position(),
            })
        }
    }

    // ========== Statement Parsing ==========

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.current() {
            Some(Token::Let) => self.parse_variable_decl(),
            Some(Token::Output) => self.parse_output(),
            Some(Token::If) => self.parse_if(),
            Some(Token::While) => self.parse_while(),
            Some(Token::Add) => self.parse_compound_assignment(BinaryOp::Add),
            Some(Token::Subtract) => self.parse_compound_assignment(BinaryOp::Subtract),
            Some(Token::Multiply) => self.parse_compound_assignment(BinaryOp::Multiply),
            Some(Token::Divide) => self.parse_compound_assignment(BinaryOp::Divide),
            Some(Token::Identifier(_)) => self.parse_assignment_or_expr(),
            _ => Err(ParseError::InvalidStatement(
                self.current_position(),
                format!("Unexpected token: {:?}", self.current()),
            )),
        }
    }

    /// Parse: let x be a decimal with value 3.14.
    fn parse_variable_decl(&mut self) -> Result<Statement, ParseError> {
        self.expect(Token::Let)?;

        let name = match self.current() {
            Some(Token::Identifier(n)) => n.clone(),
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "identifier".to_string(),
                    found: self.current().cloned().unwrap_or(Token::Period),
                    position: self.current_position(),
                })
            }
        };
        self.advance();

        self.expect(Token::Be)?;

        // Optional article (a/an)
        if self.check(&Token::A) || self.check(&Token::An) {
            self.advance();
        }

        let var_type = self.parse_type()?;

        self.expect(Token::With)?;
        self.expect(Token::Value)?;

        let value = self.parse_expression()?;

        self.expect(Token::Period)?;

        Ok(Statement::VariableDecl {
            name,
            var_type,
            value,
        })
    }

    /// Parse: output "Hello World".
    fn parse_output(&mut self) -> Result<Statement, ParseError> {
        self.expect(Token::Output)?;
        let expr = self.parse_expression()?;
        self.expect(Token::Period)?;
        Ok(Statement::Output(expr))
    }

    /// Parse: If condition then ... otherwise ...
    fn parse_if(&mut self) -> Result<Statement, ParseError> {
        self.expect(Token::If)?;

        let condition = self.parse_expression()?;

        // Optional "then"
        self.match_token(&Token::Then);

        // Optional comma
        self.match_token(&Token::Comma);

        let then_block = self.parse_block()?;

        let else_block = if self.match_token(&Token::Otherwise) || self.match_token(&Token::Else) {
            // Optional comma
            self.match_token(&Token::Comma);
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Statement::If {
            condition,
            then_block,
            else_block,
        })
    }

    /// Parse: While condition, ...
    fn parse_while(&mut self) -> Result<Statement, ParseError> {
        self.expect(Token::While)?;

        let condition = self.parse_expression()?;

        // Optional comma
        self.match_token(&Token::Comma);

        let body = self.parse_block()?;

        Ok(Statement::While { condition, body })
    }

    /// Parse compound assignment: Add 5 to x.
    fn parse_compound_assignment(&mut self, op: BinaryOp) -> Result<Statement, ParseError> {
        self.advance(); // Skip the operation keyword (Add/Subtract/etc.)

        let value = self.parse_expression()?;

        // Expect "to" or "from" depending on operation
        match op {
            BinaryOp::Add | BinaryOp::Multiply => self.expect(Token::To)?,
            BinaryOp::Subtract | BinaryOp::Divide => self.expect(Token::From)?,
            _ => {}
        }

        let name = match self.current() {
            Some(Token::Identifier(n)) => n.clone(),
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "identifier".to_string(),
                    found: self.current().cloned().unwrap_or(Token::Period),
                    position: self.current_position(),
                })
            }
        };
        self.advance();

        self.expect(Token::Period)?;

        Ok(Statement::CompoundAssignment { name, op, value })
    }

    /// Parse assignment or expression statement
    fn parse_assignment_or_expr(&mut self) -> Result<Statement, ParseError> {
        let expr = self.parse_expression()?;
        self.expect(Token::Period)?;
        Ok(Statement::ExprStatement(expr))
    }

    /// Parse a block of statements (ends at "otherwise", "else", or dedent)
    fn parse_block(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements = Vec::new();

        // For now, parse a single statement as a block
        // In the future, we could track indentation or use explicit "end" markers
        if !self.is_at_end()
            && !self.check(&Token::Otherwise)
            && !self.check(&Token::Else)
        {
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }

    /// Parse a type
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.current() {
            Some(Token::TypeText) => {
                self.advance();
                Ok(Type::Text)
            }
            Some(Token::TypeInt) => {
                self.advance();
                Ok(Type::Int)
            }
            Some(Token::TypeFloat) => {
                self.advance();
                Ok(Type::Float)
            }
            Some(Token::TypeBool) => {
                self.advance();
                Ok(Type::Bool)
            }
            Some(Token::TypeList) => {
                self.advance();
                if self.match_token(&Token::Of) {
                    let inner = self.parse_type()?;
                    Ok(Type::List(Box::new(inner)))
                } else {
                    Ok(Type::List(Box::new(Type::Int)))
                }
            }
            Some(Token::TypeDict) => {
                self.advance();
                Ok(Type::Dict(Box::new(Type::Text), Box::new(Type::Int)))
            }
            Some(Token::TypeTuple) => {
                self.advance();
                Ok(Type::Tuple(vec![]))
            }
            Some(Token::TypeSet) => {
                self.advance();
                if self.match_token(&Token::Of) {
                    let inner = self.parse_type()?;
                    Ok(Type::Set(Box::new(inner)))
                } else {
                    Ok(Type::Set(Box::new(Type::Int)))
                }
            }
            _ => Err(ParseError::ExpectedType(self.current_position())),
        }
    }

    // ========== Expression Parsing ==========

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_term()?;

        // Handle "is greater than", "is less than", "is equal to", "is not equal to"
        while self.check(&Token::Is) {
            self.advance(); // consume "is"

            let negated = self.match_token(&Token::Not);

            let op = if self.match_token(&Token::Greater) {
                self.expect(Token::Than)?;
                if negated {
                    BinaryOp::LessEq
                } else {
                    BinaryOp::Greater
                }
            } else if self.match_token(&Token::Less) {
                self.expect(Token::Than)?;
                if negated {
                    BinaryOp::GreaterEq
                } else {
                    BinaryOp::Less
                }
            } else if self.match_token(&Token::Equal) || self.match_token(&Token::Same) {
                // "is equal to" or "is same as"
                self.match_token(&Token::To);
                if negated {
                    BinaryOp::NotEqual
                } else {
                    BinaryOp::Equal
                }
            } else {
                break;
            };

            let right = self.parse_term()?;
            left = Expr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.current().cloned() {
            Some(Token::IntLiteral(n)) => {
                self.advance();
                Ok(Expr::IntLiteral(n.unwrap_or(0)))
            }
            Some(Token::FloatLiteral(n)) => {
                self.advance();
                Ok(Expr::FloatLiteral(n.unwrap_or(0.0)))
            }
            Some(Token::StringLiteral(s)) => {
                self.advance();
                Ok(Expr::StringLiteral(s))
            }
            Some(Token::True) => {
                self.advance();
                Ok(Expr::BoolLiteral(true))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Expr::BoolLiteral(false))
            }
            Some(Token::Identifier(name)) => {
                self.advance();
                Ok(Expr::Identifier(name))
            }
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            // Type conversion: "standard number of x"
            Some(Token::TypeInt) | Some(Token::TypeFloat) | Some(Token::TypeText) => {
                let target_type = self.parse_type()?;
                self.expect(Token::Of)?;
                let expr = self.parse_primary()?;
                Ok(Expr::TypeConversion {
                    target_type,
                    expr: Box::new(expr),
                })
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found: self.current().cloned().unwrap_or(Token::Period),
                position: self.current_position(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_variable_decl() {
        let program = Parser::parse("let x be a decimal with value 3.14.").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::VariableDecl {
                name,
                var_type,
                value,
            } => {
                assert_eq!(name, "x");
                assert_eq!(*var_type, Type::Float);
                match value {
                    Expr::FloatLiteral(v) => assert!((v - 3.14).abs() < 0.001),
                    _ => panic!("Expected FloatLiteral"),
                }
            }
            _ => panic!("Expected VariableDecl"),
        }
    }

    #[test]
    fn test_parse_output() {
        let program = Parser::parse(r#"output "Hello World"."#).unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Output(Expr::StringLiteral(s)) => {
                assert_eq!(s, "Hello World");
            }
            _ => panic!("Expected Output with StringLiteral"),
        }
    }
}
