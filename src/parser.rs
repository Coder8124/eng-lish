use crate::ast::*;
use crate::lexer::{Lexer, SpannedToken, Token};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEof,
    UnexpectedToken {
        expected: String,
        found: Token,
        line: usize,
    },
    InvalidStatement(usize, String),
    ExpectedType(usize),
    LexerError(usize, String),
}

/// Convert a Token to a user-friendly description
fn friendly_token_name(token: &Token) -> String {
    match token {
        Token::Let => "'let'".to_string(),
        Token::Be => "'be'".to_string(),
        Token::A => "'a'".to_string(),
        Token::An => "'an'".to_string(),
        Token::With => "'with'".to_string(),
        Token::Value => "'value'".to_string(),
        Token::Period => "a period (.)".to_string(),
        Token::Comma => "a comma (,)".to_string(),
        Token::Colon => "a colon (:)".to_string(),
        Token::If => "'if'".to_string(),
        Token::Then => "'then'".to_string(),
        Token::Else | Token::Otherwise => "'otherwise'".to_string(),
        Token::While => "'while'".to_string(),
        Token::End => "'End'".to_string(),
        Token::Output => "'output'".to_string(),
        Token::Set => "'set'".to_string(),
        Token::To => "'to'".to_string(),
        Token::The => "'the'".to_string(),
        Token::Result => "'result'".to_string(),
        Token::Of => "'of'".to_string(),
        Token::And => "'and'".to_string(),
        Token::Or => "'or'".to_string(),
        Token::Negative => "'negative'".to_string(),
        Token::Not => "'not'".to_string(),
        Token::For => "'for'".to_string(),
        Token::Each => "'each'".to_string(),
        Token::Stop => "'stop'".to_string(),
        Token::Skip => "'skip'".to_string(),
        Token::From => "'from'".to_string(),
        Token::Call => "'call'".to_string(),
        Token::Give => "'give'".to_string(),
        Token::Back => "'back'".to_string(),
        Token::Returning => "'returning'".to_string(),
        Token::Define => "'define'".to_string(),
        Token::Kind => "'kind'".to_string(),
        Token::Property => "'property'".to_string(),
        Token::Ask => "'ask'".to_string(),
        Token::IntLiteral(Some(n)) => format!("the number {}", n),
        Token::IntLiteral(None) => "a number".to_string(),
        Token::FloatLiteral(Some(f)) => format!("the decimal {}", f),
        Token::FloatLiteral(None) => "a decimal".to_string(),
        Token::StringLiteral(s) => format!("the text \"{}\"", s),
        Token::Identifier(name) => format!("'{}'", name),
        Token::True | Token::False => "a yes/no value".to_string(),
        _ => format!("{:?}", token),
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedEof => {
                write!(f, "Your program ended too early. You might be missing an 'End.' to close a block.")
            }
            ParseError::UnexpectedToken { expected, found, line } => {
                let found_name = friendly_token_name(found);
                // Make the expected description friendlier
                let expected_friendly = match expected.as_str() {
                    "Period" => "a period (.) at the end of the sentence",
                    "identifier" => "a name",
                    "expression" => "a value or expression",
                    "called" => "'called'",
                    _ => expected,
                };
                write!(f, "Line {}: Expected {}, but found {}.", line, expected_friendly, found_name)
            }
            ParseError::InvalidStatement(line, msg) => {
                write!(f, "Line {}: I don't understand this sentence. {}", line, msg)
            }
            ParseError::ExpectedType(line) => {
                write!(f, "Line {}: I need to know what type this is (like 'standard number', 'decimal', or 'text').", line)
            }
            ParseError::LexerError(line, msg) => {
                write!(f, "Line {}: {}", line, msg)
            }
        }
    }
}

impl std::error::Error for ParseError {}

pub struct Parser {
    tokens: Vec<SpannedToken>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<SpannedToken>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(source: &str) -> Result<Program, ParseError> {
        let tokens =
            Lexer::tokenize(source).map_err(|e| ParseError::LexerError(e.line, e.message))?;
        let mut parser = Parser::new(tokens);
        parser.parse_program()
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut classes = Vec::new();
        let mut functions = Vec::new();
        let mut statements = Vec::new();
        let mut statement_lines = Vec::new();

        while !self.is_at_end() {
            match self.current() {
                // "Define a kind called ..." -> class definition
                Some(Token::Define) => {
                    classes.push(self.parse_class_def()?);
                }
                // "To funcName ..." -> function definition (at top level)
                Some(Token::To) if self.is_function_def() => {
                    functions.push(self.parse_function_def()?);
                }
                _ => {
                    let line = self.current_line();
                    statements.push(self.parse_statement()?);
                    statement_lines.push(line);
                }
            }
        }

        Ok(Program {
            classes,
            functions,
            statements,
            statement_lines,
        })
    }

    /// Check if "To" starts a function definition vs compound assignment target
    /// Function def: "To <identifier> with ..." or "To <identifier> returning ..."
    fn is_function_def(&self) -> bool {
        // Look ahead: To <Identifier> (with|returning|:)
        if let Some(Token::To) = self.tokens.get(self.current).map(|t| &t.token) {
            if let Some(Token::Identifier(_)) = self.tokens.get(self.current + 1).map(|t| &t.token)
            {
                // Check what follows the identifier
                if let Some(next) = self.tokens.get(self.current + 2).map(|t| &t.token) {
                    return matches!(
                        next,
                        Token::With | Token::Returning | Token::Colon
                    );
                }
            }
        }
        false
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

    fn current_line(&self) -> usize {
        self.tokens
            .get(self.current)
            .map(|t| t.line)
            .unwrap_or(1)
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
                line: self.current_line(),
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
            Some(Token::For) => self.parse_for(),
            Some(Token::Stop) => self.parse_break(),
            Some(Token::Skip) => self.parse_continue(),
            Some(Token::Add) => self.parse_compound_assignment(BinaryOp::Add),
            Some(Token::Subtract) => self.parse_compound_assignment(BinaryOp::Subtract),
            Some(Token::Multiply) => self.parse_compound_assignment(BinaryOp::Multiply),
            Some(Token::Divide) => self.parse_compound_assignment(BinaryOp::Divide),
            Some(Token::Give) => self.parse_return(),
            Some(Token::Call) => self.parse_call_statement(),
            Some(Token::Ask) => self.parse_ask_statement(),
            Some(Token::Set) => self.parse_set_statement(),
            Some(Token::Plot) => self.parse_plot(),
            Some(Token::Identifier(_)) => self.parse_assignment_or_expr(),
            _ => Err(ParseError::InvalidStatement(
                self.current_line(),
                format!("Unexpected token: {:?}", self.current()),
            )),
        }
    }

    /// Parse: let x be a decimal with value 3.14.
    /// Also: let alice be a Person created with "Alice" and 30.
    fn parse_variable_decl(&mut self) -> Result<Statement, ParseError> {
        self.expect(Token::Let)?;

        let name = self.expect_identifier()?;

        self.expect(Token::Be)?;

        // Optional article (a/an)
        if self.check(&Token::A) || self.check(&Token::An) {
            self.advance();
        }

        let var_type = self.parse_type()?;

        // Check if this is object instantiation: "Person created with ..."
        if let Type::Class(_) = &var_type {
            if self.match_token(&Token::Created) {
                self.expect(Token::With)?;
                let arguments = self.parse_argument_list()?;
                self.expect(Token::Period)?;

                let class_name = match &var_type {
                    Type::Class(n) => n.clone(),
                    _ => unreachable!(),
                };

                return Ok(Statement::VariableDecl {
                    name,
                    var_type,
                    value: Expr::NewObject {
                        class_name,
                        arguments,
                    },
                });
            }
        }

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

    /// Parse: If condition then ... otherwise if ... otherwise ...
    fn parse_if(&mut self) -> Result<Statement, ParseError> {
        self.expect(Token::If)?;

        let condition = self.parse_expression()?;

        // Optional "then"
        self.match_token(&Token::Then);

        // Optional comma
        self.match_token(&Token::Comma);

        let then_block = self.parse_block()?;

        // Parse else-if chains
        let mut else_ifs = Vec::new();
        let mut else_block = None;

        while self.match_token(&Token::Otherwise) || self.match_token(&Token::Else) {
            // Optional comma
            self.match_token(&Token::Comma);

            // Check if this is an else-if
            if self.check(&Token::If) {
                self.advance(); // consume "if"
                let elif_condition = self.parse_expression()?;
                self.match_token(&Token::Then);
                self.match_token(&Token::Comma);
                let elif_block = self.parse_block()?;
                else_ifs.push((elif_condition, elif_block));
            } else {
                // This is the final else block
                else_block = Some(self.parse_block()?);
                break;
            }
        }

        Ok(Statement::If {
            condition,
            then_block,
            else_ifs,
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

    /// Parse: For each x from start to end, ... End.
    fn parse_for(&mut self) -> Result<Statement, ParseError> {
        self.expect(Token::For)?;
        self.expect(Token::Each)?;

        let variable = self.expect_identifier()?;

        self.expect(Token::From)?;
        let start = self.parse_expression()?;

        self.expect(Token::To)?;
        let end = self.parse_expression()?;

        // Optional comma
        self.match_token(&Token::Comma);

        // Parse body until "End."
        let body = self.parse_body_until_end()?;

        self.expect(Token::End)?;
        self.expect(Token::Period)?;

        Ok(Statement::For {
            variable,
            start,
            end,
            body,
        })
    }

    /// Parse: stop.
    fn parse_break(&mut self) -> Result<Statement, ParseError> {
        self.expect(Token::Stop)?;
        self.expect(Token::Period)?;
        Ok(Statement::Break)
    }

    /// Parse: skip.
    fn parse_continue(&mut self) -> Result<Statement, ParseError> {
        self.expect(Token::Skip)?;
        self.expect(Token::Period)?;
        Ok(Statement::Continue)
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

        let name = self.expect_identifier()?;

        self.expect(Token::Period)?;

        Ok(Statement::CompoundAssignment { name, op, value })
    }

    /// Parse assignment or expression statement
    fn parse_assignment_or_expr(&mut self) -> Result<Statement, ParseError> {
        let expr = self.parse_expression()?;
        self.expect(Token::Period)?;
        Ok(Statement::ExprStatement(expr))
    }

    // ========== Function Parsing ==========

    /// Parse: To funcName with a standard number x and a standard number y returning a standard number:
    ///     ...
    /// End.
    fn parse_function_def(&mut self) -> Result<FunctionDef, ParseError> {
        self.expect(Token::To)?;

        let name = self.expect_identifier()?;

        // Parse parameters: "with a type name and a type name ..."
        let parameters = if self.match_token(&Token::With) {
            self.parse_parameter_list()?
        } else {
            Vec::new()
        };

        // Parse return type: "returning a type" or "returning nothing"
        let return_type = if self.match_token(&Token::Returning) {
            // Optional article
            if self.check(&Token::A) || self.check(&Token::An) {
                self.advance();
            }
            if self.match_token(&Token::Nothing) {
                Type::Void
            } else {
                self.parse_type()?
            }
        } else {
            Type::Void
        };

        self.expect(Token::Colon)?;

        // Parse body until "End."
        let body = self.parse_body_until_end()?;

        self.expect(Token::End)?;
        self.expect(Token::Period)?;

        Ok(FunctionDef {
            name,
            parameters,
            return_type,
            body,
        })
    }

    /// Parse parameter list: "a standard number x and a standard number y"
    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut params = Vec::new();

        loop {
            // Optional article
            if self.check(&Token::A) || self.check(&Token::An) {
                self.advance();
            }

            let param_type = self.parse_type()?;

            let name = self.expect_identifier()?;

            params.push(Parameter {
                name,
                param_type,
            });

            if !self.match_token(&Token::And) {
                break;
            }
        }

        Ok(params)
    }

    /// Parse: Give back expr.
    fn parse_return(&mut self) -> Result<Statement, ParseError> {
        self.expect(Token::Give)?;
        self.expect(Token::Back)?;

        if self.check(&Token::Period) {
            self.advance();
            return Ok(Statement::Return(None));
        }

        let expr = self.parse_expression()?;
        self.expect(Token::Period)?;

        Ok(Statement::Return(Some(expr)))
    }

    /// Parse: Call funcName with args.
    fn parse_call_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(Token::Call)?;
        let name = self.expect_identifier()?;

        let arguments = if self.match_token(&Token::With) {
            self.parse_argument_list()?
        } else {
            Vec::new()
        };

        self.expect(Token::Period)?;

        Ok(Statement::ExprStatement(Expr::FunctionCall {
            name,
            arguments,
        }))
    }

    /// Parse: Ask obj to method. / Ask obj to method with args.
    fn parse_ask_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(Token::Ask)?;
        let object_name = self.expect_identifier()?;
        self.expect(Token::To)?;
        let method = self.expect_identifier()?;

        let arguments = if self.match_token(&Token::With) {
            self.parse_argument_list()?
        } else {
            Vec::new()
        };

        self.expect(Token::Period)?;

        Ok(Statement::ExprStatement(Expr::MethodCall {
            object: Box::new(Expr::Identifier(object_name)),
            method,
            arguments,
        }))
    }

    /// Parse: Set name to value. / Set the prop of obj to value.
    fn parse_set_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(Token::Set)?;

        // Check if this is property assignment: "Set the prop of obj to value."
        if self.match_token(&Token::The) {
            let property = self.expect_identifier()?;
            self.expect(Token::Of)?;
            let object = self.expect_identifier()?;
            self.expect(Token::To)?;
            let value = self.parse_expression()?;
            self.expect(Token::Period)?;
            return Ok(Statement::PropertyAssignment {
                object,
                property,
                value,
            });
        }

        // Regular assignment: "Set name to value."
        let name = self.expect_identifier()?;
        self.expect(Token::To)?;
        let value = self.parse_expression()?;
        self.expect(Token::Period)?;

        Ok(Statement::Assignment { name, value })
    }

    /// Parse: plot data [against yData] [as a <type> chart] [titled "Title"] to "file.html".
    fn parse_plot(&mut self) -> Result<Statement, ParseError> {
        self.expect(Token::Plot)?;

        // Parse the data expression
        let data = self.parse_expression()?;

        // Optional "against yData" for scatter plots
        let against = if self.match_token(&Token::Against) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Optional "as a <type> chart"
        let chart_type = if self.match_token(&Token::As) {
            // Optional article
            if self.check(&Token::A) || self.check(&Token::An) {
                self.advance();
            }

            let ct = if self.match_token(&Token::Line) {
                ChartType::Line
            } else if self.match_token(&Token::Bar) {
                ChartType::Bar
            } else if self.match_token(&Token::Scatter) {
                ChartType::Scatter
            } else if self.match_token(&Token::Histogram) {
                ChartType::Histogram
            } else {
                ChartType::Line // default
            };

            // Optional "chart" keyword
            self.match_token(&Token::Chart);

            // Optional "plot" keyword (for "scatter plot")
            self.match_token(&Token::Plot);

            ct
        } else {
            ChartType::Line // default chart type
        };

        // Optional "titled 'Title'"
        let title = if self.match_token(&Token::Titled) {
            match self.current().cloned() {
                Some(Token::StringLiteral(s)) => {
                    self.advance();
                    Some(s)
                }
                _ => None,
            }
        } else {
            None
        };

        // Required "to 'filename.html'"
        self.expect(Token::To)?;
        let output_file = match self.current().cloned() {
            Some(Token::StringLiteral(s)) => {
                self.advance();
                s
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "output filename".to_string(),
                    found: self.current().cloned().unwrap_or(Token::Period),
                    line: self.current_line(),
                });
            }
        };

        self.expect(Token::Period)?;

        Ok(Statement::Plot {
            data,
            against,
            chart_type,
            title,
            output_file,
        })
    }

    /// Parse argument list: "expr and expr and expr"
    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();

        // Parse arguments at comparison level (not full expression)
        // so that "and" is treated as separator, not boolean operator
        args.push(self.parse_comparison()?);

        while self.match_token(&Token::And) {
            args.push(self.parse_comparison()?);
        }

        Ok(args)
    }

    /// Parse statements until End/EndCreate/EndKind token
    fn parse_body_until_end(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements = Vec::new();

        while !self.is_at_end()
            && !self.check(&Token::End)
            && !self.check(&Token::EndCreate)
            && !self.check(&Token::EndKind)
        {
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }

    /// Helper to parse an identifier and return its name.
    /// Also accepts certain keyword tokens that can be used as variable/function names.
    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.current().cloned() {
            Some(Token::Identifier(name)) => {
                self.advance();
                Ok(name)
            }
            // Allow these keywords to be used as identifiers in name contexts
            Some(Token::Result) => {
                self.advance();
                Ok("result".to_string())
            }
            Some(Token::Value) => {
                self.advance();
                Ok("value".to_string())
            }
            Some(Token::Kind) => {
                self.advance();
                Ok("kind".to_string())
            }
            Some(Token::Property) => {
                self.advance();
                Ok("property".to_string())
            }
            other => Err(ParseError::UnexpectedToken {
                expected: "identifier".to_string(),
                found: other.unwrap_or(Token::Period),
                line: self.current_line(),
            }),
        }
    }

    // ========== Class Parsing ==========

    /// Parse: Define a kind called ClassName [that extends Parent] with the following:
    ///     Property ...
    ///     To create ...
    ///     To method ...
    /// End kind.
    fn parse_class_def(&mut self) -> Result<ClassDef, ParseError> {
        self.expect(Token::Define)?;

        // Optional article
        if self.check(&Token::A) || self.check(&Token::An) {
            self.advance();
        }

        self.expect(Token::Kind)?;

        // "called ClassName" - but "called" isn't a keyword, it will be an Identifier
        // Let me check if we have the identifier "called"
        match self.current().cloned() {
            Some(Token::Identifier(s)) if s.to_lowercase() == "called" => {
                self.advance();
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "called".to_string(),
                    found: self.current().cloned().unwrap_or(Token::Period),
                    line: self.current_line(),
                });
            }
        }

        let name = self.expect_identifier()?;

        // Optional: "that extends Parent"
        let parent = match self.current().cloned() {
            Some(Token::Identifier(s)) if s.to_lowercase() == "that" => {
                self.advance(); // consume "that"
                self.expect(Token::Extends)?;
                Some(self.expect_identifier()?)
            }
            _ => None,
        };

        self.expect(Token::With)?;
        self.expect(Token::TheFollowing)?;
        self.expect(Token::Colon)?;

        let mut properties = Vec::new();
        let mut constructor = None;
        let mut methods = Vec::new();

        // Parse class body until "End kind."
        while !self.is_at_end() && !self.check(&Token::EndKind) {
            match self.current() {
                Some(Token::Property) => {
                    properties.push(self.parse_property()?);
                }
                Some(Token::To) => {
                    // Distinguish constructor from method
                    // Constructor: "To create with ..."
                    // Method: "To methodName ..."
                    if self.is_constructor() {
                        constructor = Some(self.parse_constructor()?);
                    } else {
                        methods.push(self.parse_method()?);
                    }
                }
                _ => {
                    return Err(ParseError::InvalidStatement(
                        self.current_line(),
                        format!(
                            "Expected Property, To, or End kind in class body, found {:?}",
                            self.current()
                        ),
                    ));
                }
            }
        }

        self.expect(Token::EndKind)?;
        self.expect(Token::Period)?;

        Ok(ClassDef {
            name,
            parent,
            properties,
            constructor,
            methods,
        })
    }

    /// Check if current position is a constructor: "To create ..."
    fn is_constructor(&self) -> bool {
        if let Some(Token::To) = self.tokens.get(self.current).map(|t| &t.token) {
            if let Some(Token::Create) = self.tokens.get(self.current + 1).map(|t| &t.token) {
                return true;
            }
        }
        false
    }

    /// Parse: Property name is a type.
    fn parse_property(&mut self) -> Result<Property, ParseError> {
        self.expect(Token::Property)?;
        let name = self.expect_identifier()?;
        self.expect(Token::Is)?;

        // Optional article
        if self.check(&Token::A) || self.check(&Token::An) {
            self.advance();
        }

        let prop_type = self.parse_type()?;
        self.expect(Token::Period)?;

        Ok(Property {
            name,
            prop_type,
        })
    }

    /// Parse: To create with a type param and a type param:
    ///     ...
    /// End create.
    fn parse_constructor(&mut self) -> Result<Constructor, ParseError> {
        self.expect(Token::To)?;
        self.expect(Token::Create)?;

        let parameters = if self.match_token(&Token::With) {
            self.parse_parameter_list()?
        } else {
            Vec::new()
        };

        self.expect(Token::Colon)?;

        // Parse body until "End create."
        let mut body = Vec::new();
        while !self.is_at_end() && !self.check(&Token::EndCreate) {
            body.push(self.parse_statement()?);
        }

        self.expect(Token::EndCreate)?;
        self.expect(Token::Period)?;

        Ok(Constructor { parameters, body })
    }

    /// Parse a method within a class (same as function def but inside class)
    /// To methodName with params returning type:
    ///     ...
    /// End.
    fn parse_method(&mut self) -> Result<Method, ParseError> {
        self.expect(Token::To)?;
        let name = self.expect_identifier()?;

        let parameters = if self.match_token(&Token::With) {
            self.parse_parameter_list()?
        } else {
            Vec::new()
        };

        let return_type = if self.match_token(&Token::Returning) {
            // Optional article
            if self.check(&Token::A) || self.check(&Token::An) {
                self.advance();
            }
            if self.match_token(&Token::Nothing) {
                Type::Void
            } else {
                self.parse_type()?
            }
        } else {
            Type::Void
        };

        self.expect(Token::Colon)?;

        let body = self.parse_body_until_end()?;

        self.expect(Token::End)?;
        self.expect(Token::Period)?;

        Ok(Method {
            name,
            parameters,
            return_type,
            body,
        })
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
            // Class type: identifier that's not a keyword
            Some(Token::Identifier(name)) => {
                let class_name = name.clone();
                self.advance();
                Ok(Type::Class(class_name))
            }
            _ => Err(ParseError::ExpectedType(self.current_line())),
        }
    }

    // ========== Expression Parsing ==========
    // Precedence (lowest to highest):
    // 1. Or
    // 2. And
    // 3. Comparison (is greater than, is less than, is equal to, etc.)
    // 4. Term (remainder, quotient)
    // 5. Unary (not, negative)
    // 6. Primary (literals, identifiers, function calls, etc.)

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_or()
    }

    /// Parse: x or y
    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_and()?;

        while self.match_token(&Token::Or) {
            let right = self.parse_and()?;
            left = Expr::BinaryOp {
                op: BinaryOp::Or,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse: x and y
    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_comparison()?;

        while self.match_token(&Token::And) {
            let right = self.parse_comparison()?;
            left = Expr::BinaryOp {
                op: BinaryOp::And,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
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

    /// Parse: remainder x by y, quotient x by y
    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        // Check for "remainder x by y" or "quotient x by y"
        if self.check(&Token::Remainder) {
            self.advance();
            let left = self.parse_unary()?;
            self.expect(Token::By)?;
            let right = self.parse_unary()?;
            return Ok(Expr::BinaryOp {
                op: BinaryOp::Remainder,
                left: Box::new(left),
                right: Box::new(right),
            });
        }

        if self.check(&Token::Quotient) {
            self.advance();
            let left = self.parse_unary()?;
            self.expect(Token::By)?;
            let right = self.parse_unary()?;
            return Ok(Expr::BinaryOp {
                op: BinaryOp::Quotient,
                left: Box::new(left),
                right: Box::new(right),
            });
        }

        self.parse_unary()
    }

    /// Parse: not x, negative x
    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(&Token::Not) {
            let operand = self.parse_unary()?;
            return Ok(Expr::UnaryOp {
                op: UnaryOp::Not,
                operand: Box::new(operand),
            });
        }

        if self.match_token(&Token::Negative) {
            let operand = self.parse_unary()?;
            return Ok(Expr::UnaryOp {
                op: UnaryOp::Negate,
                operand: Box::new(operand),
            });
        }

        self.parse_postfix()
    }

    /// Parse postfix expressions: arr[0], arr[1][2], etc.
    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        // Handle index access: expr[index]
        while self.match_token(&Token::LBracket) {
            let index = self.parse_expression()?;
            self.expect(Token::RBracket)?;
            expr = Expr::Index {
                collection: Box::new(expr),
                index: Box::new(index),
            };
        }

        Ok(expr)
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
                // Check for "created with" -> object instantiation
                if self.check(&Token::Created) {
                    self.advance(); // consume "created"
                    self.expect(Token::With)?;
                    let arguments = self.parse_argument_list()?;
                    return Ok(Expr::NewObject {
                        class_name: name,
                        arguments,
                    });
                }
                Ok(Expr::Identifier(name))
            }
            // Keywords that can also be used as identifiers in expression context
            Some(Token::Result) => {
                self.advance();
                Ok(Expr::Identifier("result".to_string()))
            }
            Some(Token::Value) => {
                self.advance();
                Ok(Expr::Identifier("value".to_string()))
            }
            Some(Token::Kind) => {
                self.advance();
                Ok(Expr::Identifier("kind".to_string()))
            }
            Some(Token::Property) => {
                self.advance();
                Ok(Expr::Identifier("property".to_string()))
            }
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            // List literal: [1, 2, 3]
            Some(Token::LBracket) => {
                self.advance(); // consume '['
                let mut elements = Vec::new();

                if !self.check(&Token::RBracket) {
                    elements.push(self.parse_expression()?);
                    while self.match_token(&Token::Comma) {
                        elements.push(self.parse_expression()?);
                    }
                }

                self.expect(Token::RBracket)?;
                Ok(Expr::ListLiteral(elements))
            }
            // "the result of funcName with args" or "the result of asking obj to method"
            // "the prop of obj"
            Some(Token::The) => {
                self.advance(); // consume "the"
                self.parse_the_expression()
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
                line: self.current_line(),
            }),
        }
    }

    /// Parse expressions starting with "the":
    /// - "the result of funcName with args"
    /// - "the result of asking obj to method"
    /// - "the prop of obj"
    fn parse_the_expression(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(&Token::Result) {
            self.expect(Token::Of)?;

            // "the result of asking obj to method with args"
            if self.match_token(&Token::Asking) {
                let object_name = self.expect_identifier()?;
                self.expect(Token::To)?;
                let method = self.expect_identifier()?;

                let arguments = if self.match_token(&Token::With) {
                    self.parse_argument_list()?
                } else {
                    Vec::new()
                };

                return Ok(Expr::MethodCall {
                    object: Box::new(Expr::Identifier(object_name)),
                    method,
                    arguments,
                });
            }

            // "the result of funcName with args"
            let name = self.expect_identifier()?;

            let arguments = if self.match_token(&Token::With) {
                self.parse_argument_list()?
            } else {
                Vec::new()
            };

            Ok(Expr::FunctionCall { name, arguments })
        } else {
            // "the prop of obj" -> property access
            let property = self.expect_identifier()?;
            self.expect(Token::Of)?;
            let object_name = self.expect_identifier()?;

            Ok(Expr::PropertyAccess {
                object: Box::new(Expr::Identifier(object_name)),
                property,
            })
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

    #[test]
    fn test_parse_operators() {
        // Test remainder
        let program = Parser::parse("output remainder 10 by 3.").unwrap();
        match &program.statements[0] {
            Statement::Output(Expr::BinaryOp { op, .. }) => {
                assert_eq!(*op, BinaryOp::Remainder);
            }
            _ => panic!("Expected Output with BinaryOp Remainder"),
        }

        // Test quotient
        let program = Parser::parse("output quotient 10 by 3.").unwrap();
        match &program.statements[0] {
            Statement::Output(Expr::BinaryOp { op, .. }) => {
                assert_eq!(*op, BinaryOp::Quotient);
            }
            _ => panic!("Expected Output with BinaryOp Quotient"),
        }

        // Test not
        let program = Parser::parse("output not true.").unwrap();
        match &program.statements[0] {
            Statement::Output(Expr::UnaryOp { op, .. }) => {
                assert_eq!(*op, UnaryOp::Not);
            }
            _ => panic!("Expected Output with UnaryOp Not"),
        }

        // Test negative
        let program = Parser::parse("output negative 42.").unwrap();
        match &program.statements[0] {
            Statement::Output(Expr::UnaryOp { op, .. }) => {
                assert_eq!(*op, UnaryOp::Negate);
            }
            _ => panic!("Expected Output with UnaryOp Negate"),
        }

        // Test and
        let program = Parser::parse("output true and false.").unwrap();
        match &program.statements[0] {
            Statement::Output(Expr::BinaryOp { op, .. }) => {
                assert_eq!(*op, BinaryOp::And);
            }
            _ => panic!("Expected Output with BinaryOp And"),
        }

        // Test or
        let program = Parser::parse("output true or false.").unwrap();
        match &program.statements[0] {
            Statement::Output(Expr::BinaryOp { op, .. }) => {
                assert_eq!(*op, BinaryOp::Or);
            }
            _ => panic!("Expected Output with BinaryOp Or"),
        }
    }

    #[test]
    fn test_list_literal() {
        let result = Parser::parse("output [1, 2, 3].");
        println!("Parse result: {:?}", result);
        let program = result.unwrap();
        match &program.statements[0] {
            Statement::Output(Expr::ListLiteral(elements)) => {
                assert_eq!(elements.len(), 3);
            }
            other => panic!("Expected Output with ListLiteral, got {:?}", other),
        }
    }
}
