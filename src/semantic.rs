use crate::ast::*;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum SemanticError {
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch { expected: Type, found: Type },

    #[error("Cannot apply operator {op:?} to types {left} and {right}")]
    InvalidOperator {
        op: BinaryOp,
        left: Type,
        right: Type,
    },

    #[error("Variable already declared: {0}")]
    AlreadyDeclared(String),

    #[error("Cannot convert {from} to {to}")]
    InvalidConversion { from: Type, to: Type },
}

/// Symbol table entry
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub symbol_type: Type,
    pub is_mutable: bool,
}

/// Semantic analyzer with symbol table
pub struct SemanticAnalyzer {
    /// Stack of scopes (for nested blocks)
    scopes: Vec<HashMap<String, Symbol>>,
    /// Collected errors
    pub errors: Vec<SemanticError>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            errors: Vec::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare_variable(&mut self, name: &str, var_type: Type) -> Result<(), SemanticError> {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(name) {
            return Err(SemanticError::AlreadyDeclared(name.to_string()));
        }
        scope.insert(
            name.to_string(),
            Symbol {
                name: name.to_string(),
                symbol_type: var_type,
                is_mutable: true,
            },
        );
        Ok(())
    }

    fn lookup_variable(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    /// Analyze a complete program
    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<SemanticError>> {
        for stmt in &program.statements {
            if let Err(e) = self.analyze_statement(stmt) {
                self.errors.push(e);
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn analyze_statement(&mut self, stmt: &Statement) -> Result<(), SemanticError> {
        match stmt {
            Statement::VariableDecl {
                name,
                var_type,
                value,
            } => {
                let value_type = self.analyze_expression(value)?;
                self.check_type_compatible(var_type, &value_type)?;
                self.declare_variable(name, var_type.clone())?;
            }

            Statement::Assignment { name, value } => {
                let symbol = self
                    .lookup_variable(name)
                    .ok_or_else(|| SemanticError::UndefinedVariable(name.clone()))?;
                let expected_type = symbol.symbol_type.clone();
                let value_type = self.analyze_expression(value)?;
                self.check_type_compatible(&expected_type, &value_type)?;
            }

            Statement::CompoundAssignment { name, op, value } => {
                let symbol = self
                    .lookup_variable(name)
                    .ok_or_else(|| SemanticError::UndefinedVariable(name.clone()))?;
                let var_type = symbol.symbol_type.clone();
                let value_type = self.analyze_expression(value)?;
                self.check_binary_op_types(op, &var_type, &value_type)?;
            }

            Statement::Output(expr) => {
                self.analyze_expression(expr)?;
            }

            Statement::If {
                condition,
                then_block,
                else_block,
            } => {
                let cond_type = self.analyze_expression(condition)?;
                if cond_type != Type::Bool {
                    return Err(SemanticError::TypeMismatch {
                        expected: Type::Bool,
                        found: cond_type,
                    });
                }

                self.push_scope();
                for stmt in then_block {
                    self.analyze_statement(stmt)?;
                }
                self.pop_scope();

                if let Some(else_stmts) = else_block {
                    self.push_scope();
                    for stmt in else_stmts {
                        self.analyze_statement(stmt)?;
                    }
                    self.pop_scope();
                }
            }

            Statement::While { condition, body } => {
                let cond_type = self.analyze_expression(condition)?;
                if cond_type != Type::Bool {
                    return Err(SemanticError::TypeMismatch {
                        expected: Type::Bool,
                        found: cond_type,
                    });
                }

                self.push_scope();
                for stmt in body {
                    self.analyze_statement(stmt)?;
                }
                self.pop_scope();
            }

            Statement::ExprStatement(expr) => {
                self.analyze_expression(expr)?;
            }
        }

        Ok(())
    }

    fn analyze_expression(&mut self, expr: &Expr) -> Result<Type, SemanticError> {
        match expr {
            Expr::IntLiteral(_) => Ok(Type::Int),
            Expr::FloatLiteral(_) => Ok(Type::Float),
            Expr::StringLiteral(_) => Ok(Type::Text),
            Expr::BoolLiteral(_) => Ok(Type::Bool),

            Expr::Identifier(name) => {
                let symbol = self
                    .lookup_variable(name)
                    .ok_or_else(|| SemanticError::UndefinedVariable(name.clone()))?;
                Ok(symbol.symbol_type.clone())
            }

            Expr::BinaryOp { op, left, right } => {
                let left_type = self.analyze_expression(left)?;
                let right_type = self.analyze_expression(right)?;
                self.check_binary_op_types(op, &left_type, &right_type)
            }

            Expr::UnaryOp { op, operand } => {
                let operand_type = self.analyze_expression(operand)?;
                match op {
                    UnaryOp::Negate => {
                        if operand_type == Type::Int || operand_type == Type::Float {
                            Ok(operand_type)
                        } else {
                            Err(SemanticError::TypeMismatch {
                                expected: Type::Int,
                                found: operand_type,
                            })
                        }
                    }
                    UnaryOp::Not => {
                        if operand_type == Type::Bool {
                            Ok(Type::Bool)
                        } else {
                            Err(SemanticError::TypeMismatch {
                                expected: Type::Bool,
                                found: operand_type,
                            })
                        }
                    }
                }
            }

            Expr::TypeConversion { target_type, expr } => {
                let source_type = self.analyze_expression(expr)?;
                self.check_conversion(&source_type, target_type)?;
                Ok(target_type.clone())
            }

            Expr::ListLiteral(elements) => {
                if elements.is_empty() {
                    Ok(Type::List(Box::new(Type::Int)))
                } else {
                    let first_type = self.analyze_expression(&elements[0])?;
                    for elem in &elements[1..] {
                        let elem_type = self.analyze_expression(elem)?;
                        if elem_type != first_type {
                            return Err(SemanticError::TypeMismatch {
                                expected: first_type,
                                found: elem_type,
                            });
                        }
                    }
                    Ok(Type::List(Box::new(first_type)))
                }
            }

            Expr::Index { collection, index } => {
                let coll_type = self.analyze_expression(collection)?;
                let idx_type = self.analyze_expression(index)?;

                match coll_type {
                    Type::List(inner) => {
                        if idx_type != Type::Int {
                            return Err(SemanticError::TypeMismatch {
                                expected: Type::Int,
                                found: idx_type,
                            });
                        }
                        Ok(*inner)
                    }
                    Type::Text => {
                        if idx_type != Type::Int {
                            return Err(SemanticError::TypeMismatch {
                                expected: Type::Int,
                                found: idx_type,
                            });
                        }
                        Ok(Type::Text)
                    }
                    _ => Err(SemanticError::TypeMismatch {
                        expected: Type::List(Box::new(Type::Int)),
                        found: coll_type,
                    }),
                }
            }
        }
    }

    fn check_binary_op_types(
        &self,
        op: &BinaryOp,
        left: &Type,
        right: &Type,
    ) -> Result<Type, SemanticError> {
        match op {
            BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                if (left == &Type::Int || left == &Type::Float)
                    && (right == &Type::Int || right == &Type::Float)
                {
                    if left == &Type::Float || right == &Type::Float {
                        Ok(Type::Float)
                    } else {
                        Ok(Type::Int)
                    }
                } else if left == &Type::Text && right == &Type::Text && op == &BinaryOp::Add {
                    Ok(Type::Text)
                } else {
                    Err(SemanticError::InvalidOperator {
                        op: op.clone(),
                        left: left.clone(),
                        right: right.clone(),
                    })
                }
            }
            BinaryOp::Remainder | BinaryOp::Quotient => {
                if left == &Type::Int && right == &Type::Int {
                    Ok(Type::Int)
                } else {
                    Err(SemanticError::InvalidOperator {
                        op: op.clone(),
                        left: left.clone(),
                        right: right.clone(),
                    })
                }
            }
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::Greater
            | BinaryOp::Less
            | BinaryOp::GreaterEq
            | BinaryOp::LessEq => {
                if left == right {
                    Ok(Type::Bool)
                } else {
                    Err(SemanticError::InvalidOperator {
                        op: op.clone(),
                        left: left.clone(),
                        right: right.clone(),
                    })
                }
            }
            BinaryOp::And | BinaryOp::Or => {
                if left == &Type::Bool && right == &Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(SemanticError::InvalidOperator {
                        op: op.clone(),
                        left: left.clone(),
                        right: right.clone(),
                    })
                }
            }
        }
    }

    fn check_type_compatible(&self, expected: &Type, found: &Type) -> Result<(), SemanticError> {
        if expected == found {
            Ok(())
        } else {
            Err(SemanticError::TypeMismatch {
                expected: expected.clone(),
                found: found.clone(),
            })
        }
    }

    fn check_conversion(&self, from: &Type, to: &Type) -> Result<(), SemanticError> {
        let valid = matches!(
            (from, to),
            (Type::Int, Type::Float)
                | (Type::Int, Type::Text)
                | (Type::Float, Type::Int)
                | (Type::Float, Type::Text)
                | (Type::Text, Type::Int)
                | (Type::Text, Type::Float)
                | (Type::Bool, Type::Text)
        );

        if valid || from == to {
            Ok(())
        } else {
            Err(SemanticError::InvalidConversion {
                from: from.clone(),
                to: to.clone(),
            })
        }
    }
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn analyze(source: &str) -> Result<(), Vec<SemanticError>> {
        let program = Parser::parse(source).unwrap();
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&program)
    }

    #[test]
    fn test_valid_declaration() {
        assert!(analyze("let x be a standard number with value 42.").is_ok());
        assert!(analyze("let x be a decimal with value 3.14.").is_ok());
    }

    #[test]
    fn test_type_mismatch() {
        let result = analyze("let x be a standard number with value 3.14.");
        assert!(result.is_err());
    }
}
