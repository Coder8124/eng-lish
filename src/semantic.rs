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

    #[error("Undefined function: {0}")]
    UndefinedFunction(String),

    #[error("Function {0} expects {1} arguments, but {2} were provided")]
    ArgumentCountMismatch(String, usize, usize),

    #[error("Function {0} already defined")]
    FunctionAlreadyDefined(String),

    #[error("Undefined class: {0}")]
    UndefinedClass(String),

    #[error("Class {0} already defined")]
    ClassAlreadyDefined(String),

    #[error("Undefined property {0} on class {1}")]
    UndefinedProperty(String, String),

    #[error("Undefined method {0} on class {1}")]
    UndefinedMethod(String, String),

    #[error("Return outside of function")]
    ReturnOutsideFunction,
}

/// Function signature for the symbol table
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub parameters: Vec<(String, Type)>,
    pub return_type: Type,
}

/// Class signature for the symbol table
#[derive(Debug, Clone)]
pub struct ClassSignature {
    pub name: String,
    pub parent: Option<String>,
    pub properties: HashMap<String, Type>,
    pub methods: HashMap<String, FunctionSignature>,
    pub constructor_params: Vec<(String, Type)>,
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
    /// Function signatures
    functions: HashMap<String, FunctionSignature>,
    /// Class signatures
    classes: HashMap<String, ClassSignature>,
    /// Current class being analyzed (for implicit self)
    current_class: Option<String>,
    /// Current function return type (for return statement checking)
    current_function_return_type: Option<Type>,
    /// Collected errors
    pub errors: Vec<SemanticError>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            functions: HashMap::new(),
            classes: HashMap::new(),
            current_class: None,
            current_function_return_type: None,
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
        // First pass: register all classes
        for class in &program.classes {
            if let Err(e) = self.register_class(class) {
                self.errors.push(e);
            }
        }

        // Second pass: register all functions
        for func in &program.functions {
            if let Err(e) = self.register_function(func) {
                self.errors.push(e);
            }
        }

        // Third pass: analyze class bodies
        for class in &program.classes {
            if let Err(e) = self.analyze_class(class) {
                self.errors.push(e);
            }
        }

        // Fourth pass: analyze function bodies
        for func in &program.functions {
            if let Err(e) = self.analyze_function(func) {
                self.errors.push(e);
            }
        }

        // Analyze top-level statements
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

    fn register_function(&mut self, func: &FunctionDef) -> Result<(), SemanticError> {
        if self.functions.contains_key(&func.name) {
            return Err(SemanticError::FunctionAlreadyDefined(func.name.clone()));
        }
        self.functions.insert(
            func.name.clone(),
            FunctionSignature {
                name: func.name.clone(),
                parameters: func
                    .parameters
                    .iter()
                    .map(|p| (p.name.clone(), p.param_type.clone()))
                    .collect(),
                return_type: func.return_type.clone(),
            },
        );
        Ok(())
    }

    fn analyze_function(&mut self, func: &FunctionDef) -> Result<(), SemanticError> {
        self.push_scope();
        self.current_function_return_type = Some(func.return_type.clone());

        for param in &func.parameters {
            self.declare_variable(&param.name, param.param_type.clone())?;
        }

        for stmt in &func.body {
            self.analyze_statement(stmt)?;
        }

        self.current_function_return_type = None;
        self.pop_scope();
        Ok(())
    }

    fn register_class(&mut self, class: &ClassDef) -> Result<(), SemanticError> {
        if self.classes.contains_key(&class.name) {
            return Err(SemanticError::ClassAlreadyDefined(class.name.clone()));
        }

        // Collect parent properties/methods if inheriting
        let mut properties = HashMap::new();
        let mut methods = HashMap::new();

        if let Some(parent_name) = &class.parent {
            if let Some(parent_sig) = self.classes.get(parent_name).cloned() {
                properties.extend(parent_sig.properties);
                methods.extend(parent_sig.methods);
            } else {
                return Err(SemanticError::UndefinedClass(parent_name.clone()));
            }
        }

        // Add own properties (may override parent's)
        for prop in &class.properties {
            properties.insert(prop.name.clone(), prop.prop_type.clone());
        }

        // Add own methods (may override parent's)
        for method in &class.methods {
            methods.insert(
                method.name.clone(),
                FunctionSignature {
                    name: method.name.clone(),
                    parameters: method
                        .parameters
                        .iter()
                        .map(|p| (p.name.clone(), p.param_type.clone()))
                        .collect(),
                    return_type: method.return_type.clone(),
                },
            );
        }

        let constructor_params = class
            .constructor
            .as_ref()
            .map(|c| {
                c.parameters
                    .iter()
                    .map(|p| (p.name.clone(), p.param_type.clone()))
                    .collect()
            })
            .unwrap_or_default();

        self.classes.insert(
            class.name.clone(),
            ClassSignature {
                name: class.name.clone(),
                parent: class.parent.clone(),
                properties,
                methods,
                constructor_params,
            },
        );

        Ok(())
    }

    fn analyze_class(&mut self, class: &ClassDef) -> Result<(), SemanticError> {
        self.current_class = Some(class.name.clone());

        // Get all properties for this class (including inherited)
        let all_properties = self
            .classes
            .get(&class.name)
            .map(|sig| sig.properties.clone())
            .unwrap_or_default();

        // Analyze constructor
        if let Some(constructor) = &class.constructor {
            self.push_scope();

            // Add all properties to scope (implicit self)
            for (prop_name, prop_type) in &all_properties {
                self.declare_variable(prop_name, prop_type.clone())?;
            }

            // Add constructor params
            for param in &constructor.parameters {
                // Override if same name as property
                let scope = self.scopes.last_mut().unwrap();
                scope.insert(
                    param.name.clone(),
                    Symbol {
                        name: param.name.clone(),
                        symbol_type: param.param_type.clone(),
                        is_mutable: true,
                    },
                );
            }

            for stmt in &constructor.body {
                self.analyze_statement(stmt)?;
            }

            self.pop_scope();
        }

        // Analyze methods
        for method in &class.methods {
            self.push_scope();
            self.current_function_return_type = Some(method.return_type.clone());

            // Add all properties to scope (implicit self)
            for (prop_name, prop_type) in &all_properties {
                self.declare_variable(prop_name, prop_type.clone())?;
            }

            for param in &method.parameters {
                let scope = self.scopes.last_mut().unwrap();
                scope.insert(
                    param.name.clone(),
                    Symbol {
                        name: param.name.clone(),
                        symbol_type: param.param_type.clone(),
                        is_mutable: true,
                    },
                );
            }

            for stmt in &method.body {
                self.analyze_statement(stmt)?;
            }

            self.current_function_return_type = None;
            self.pop_scope();
        }

        self.current_class = None;
        Ok(())
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

            Statement::Return(expr) => {
                if self.current_function_return_type.is_none() {
                    return Err(SemanticError::ReturnOutsideFunction);
                }
                if let Some(return_expr) = expr {
                    let return_type = self.analyze_expression(return_expr)?;
                    let expected = self.current_function_return_type.clone().unwrap();
                    if expected != Type::Void {
                        self.check_type_compatible(&expected, &return_type)?;
                    }
                }
            }

            Statement::PropertyAssignment {
                object,
                property,
                value,
            } => {
                let symbol = self
                    .lookup_variable(object)
                    .ok_or_else(|| SemanticError::UndefinedVariable(object.clone()))?;
                let class_name = match &symbol.symbol_type {
                    Type::Class(name) => name.clone(),
                    other => {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::Class("any".to_string()),
                            found: other.clone(),
                        })
                    }
                };
                let class_sig = self
                    .classes
                    .get(&class_name)
                    .ok_or_else(|| SemanticError::UndefinedClass(class_name.clone()))?;
                let prop_type = class_sig
                    .properties
                    .get(property)
                    .ok_or_else(|| {
                        SemanticError::UndefinedProperty(property.clone(), class_name.clone())
                    })?
                    .clone();
                let value_type = self.analyze_expression(value)?;
                self.check_type_compatible(&prop_type, &value_type)?;
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

            Expr::FunctionCall { name, arguments } => {
                let sig = self
                    .functions
                    .get(name)
                    .ok_or_else(|| SemanticError::UndefinedFunction(name.clone()))?
                    .clone();

                if arguments.len() != sig.parameters.len() {
                    return Err(SemanticError::ArgumentCountMismatch(
                        name.clone(),
                        sig.parameters.len(),
                        arguments.len(),
                    ));
                }

                for (arg, (_param_name, param_type)) in arguments.iter().zip(sig.parameters.iter())
                {
                    let arg_type = self.analyze_expression(arg)?;
                    self.check_type_compatible(param_type, &arg_type)?;
                }

                Ok(sig.return_type)
            }

            Expr::NewObject {
                class_name,
                arguments,
            } => {
                let class_sig = self
                    .classes
                    .get(class_name)
                    .ok_or_else(|| SemanticError::UndefinedClass(class_name.clone()))?
                    .clone();

                if arguments.len() != class_sig.constructor_params.len() {
                    return Err(SemanticError::ArgumentCountMismatch(
                        class_name.clone(),
                        class_sig.constructor_params.len(),
                        arguments.len(),
                    ));
                }

                for (arg, (_param_name, param_type)) in
                    arguments.iter().zip(class_sig.constructor_params.iter())
                {
                    let arg_type = self.analyze_expression(arg)?;
                    self.check_type_compatible(param_type, &arg_type)?;
                }

                Ok(Type::Class(class_name.clone()))
            }

            Expr::MethodCall {
                object,
                method,
                arguments,
            } => {
                let object_type = self.analyze_expression(object)?;
                let class_name = match &object_type {
                    Type::Class(name) => name.clone(),
                    other => {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::Class("any".to_string()),
                            found: other.clone(),
                        })
                    }
                };

                let class_sig = self
                    .classes
                    .get(&class_name)
                    .ok_or_else(|| SemanticError::UndefinedClass(class_name.clone()))?
                    .clone();

                let method_sig = class_sig
                    .methods
                    .get(method)
                    .ok_or_else(|| {
                        SemanticError::UndefinedMethod(method.clone(), class_name.clone())
                    })?
                    .clone();

                if arguments.len() != method_sig.parameters.len() {
                    return Err(SemanticError::ArgumentCountMismatch(
                        format!("{}.{}", class_name, method),
                        method_sig.parameters.len(),
                        arguments.len(),
                    ));
                }

                for (arg, (_param_name, param_type)) in
                    arguments.iter().zip(method_sig.parameters.iter())
                {
                    let arg_type = self.analyze_expression(arg)?;
                    self.check_type_compatible(param_type, &arg_type)?;
                }

                Ok(method_sig.return_type)
            }

            Expr::PropertyAccess { object, property } => {
                let object_type = self.analyze_expression(object)?;
                let class_name = match &object_type {
                    Type::Class(name) => name.clone(),
                    other => {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::Class("any".to_string()),
                            found: other.clone(),
                        })
                    }
                };

                let class_sig = self
                    .classes
                    .get(&class_name)
                    .ok_or_else(|| SemanticError::UndefinedClass(class_name.clone()))?;

                let prop_type = class_sig
                    .properties
                    .get(property)
                    .ok_or_else(|| {
                        SemanticError::UndefinedProperty(property.clone(), class_name.clone())
                    })?
                    .clone();

                Ok(prop_type)
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
