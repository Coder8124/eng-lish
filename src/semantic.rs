use crate::ast::*;
use crate::stdlib;
use std::collections::{HashMap, HashSet};
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum SemanticError {
    #[error(
        "Line {1}: I don't know what '{0}' is. Did you forget to create it with 'let {0} be...'?"
    )]
    UndefinedVariable(String, usize),

    #[error("Line {line}: This expects a {expected}, but you gave it a {found}.")]
    TypeMismatch {
        expected: Type,
        found: Type,
        line: usize,
    },

    #[error("Line {line}: You can't use {op:?} with {left} and {right}.")]
    InvalidOperator {
        op: BinaryOp,
        left: Type,
        right: Type,
        line: usize,
    },

    #[error(
        "Line {1}: You already created '{0}'. Use 'set {0} to...' to change its value, or pick a different name."
    )]
    AlreadyDeclared(String, usize),

    #[error("Line {line}: I can't convert {from} to {to}.")]
    InvalidConversion { from: Type, to: Type, line: usize },

    #[error(
        "Line {1}: There's no function called '{0}'. Check the spelling, or define it with 'To {0}...'"
    )]
    UndefinedFunction(String, usize),

    #[error("Line {3}: '{0}' needs {1} value(s), but you gave it {2}.")]
    ArgumentCountMismatch(String, usize, usize, usize),

    #[error("Line {1}: The function '{0}' is already defined.")]
    FunctionAlreadyDefined(String, usize),

    #[error("Line {1}: There's no class called '{0}'.")]
    UndefinedClass(String, usize),

    #[error("Line {1}: The class '{0}' is already defined.")]
    ClassAlreadyDefined(String, usize),

    #[error("Line {2}: The class '{1}' doesn't have a property called '{0}'.")]
    UndefinedProperty(String, String, usize),

    #[error("Line {2}: The class '{1}' doesn't have a method called '{0}'.")]
    UndefinedMethod(String, String, usize),

    #[error("Line {0}: 'give back' can only be used inside a function.")]
    ReturnOutsideFunction(usize),

    #[error("Line {0}: 'stop' can only be used inside a loop (while or for each).")]
    BreakOutsideLoop(usize),

    #[error("Line {0}: 'skip' can only be used inside a loop (while or for each).")]
    ContinueOutsideLoop(usize),

    #[error(
        "Line {1}: '{0}' is worked out from a watched decimal, so it has to be a watched decimal too. Try `let {0} be a watched decimal with value ...`."
    )]
    NeedsWatched(String, usize),

    #[error(
        "Line {2}: Only watched decimals have {0}, but this is a {1}. Make it with `let ... be a watched decimal with value ...` so eng-lish can follow how it was worked out."
    )]
    NotWatched(String, Type, usize),

    #[error(
        "Line {1}: A watched decimal only has a 'value' and a 'gradient', not a '{0}'."
    )]
    NoWatchedProperty(String, usize),

    #[error("Line {2}: A {0} can't go inside {1} yet. Keep each one in its own variable instead.")]
    WatchedNotAllowed(Type, String, usize),

    #[error(
        "Line {1}: A tensor has a 'shape', 'sum', 'mean', 'transpose' and 'value', but not a '{0}'."
    )]
    NoTensorProperty(String, usize),
}

impl SemanticError {
    pub fn to_beginner_string(&self) -> String {
        let (line_part, description, try_hint) = match self {
            SemanticError::UndefinedVariable(name, line) => (
                format!("Line {line}"),
                format!("'{name}' hasn't been created yet."),
                format!("Add `let {name} be a standard number with value ...` before this line."),
            ),
            SemanticError::TypeMismatch {
                expected,
                found,
                line,
            } => (
                format!("Line {line}"),
                format!("You're using a {found} where a {expected} is expected."),
                "Check that you're passing the right kind of value.".to_string(),
            ),
            SemanticError::ArgumentCountMismatch(name, expected, found, line) => (
                format!("Line {line}"),
                format!("'{name}' needs {expected} input(s), but you gave it {found}."),
                format!("Check how many inputs '{name}' takes."),
            ),
            SemanticError::UndefinedFunction(name, line) => (
                format!("Line {line}"),
                format!("There's no function called '{name}'."),
                format!("Check the spelling, or define it with `To {name} ...:`"),
            ),
            SemanticError::AlreadyDeclared(name, line) => (
                format!("Line {line}"),
                format!("'{name}' was already created."),
                format!("Use `Set {name} to ...` to change its value, or pick a different name."),
            ),
            SemanticError::ReturnOutsideFunction(line) => (
                format!("Line {line}"),
                "'Give back' can only be used inside a function.".to_string(),
                "Move this line inside a `To ... :` block.".to_string(),
            ),
            SemanticError::BreakOutsideLoop(line) => (
                format!("Line {line}"),
                "'stop' can only be used inside a loop.".to_string(),
                "Move this line inside a `For each` or `While` block.".to_string(),
            ),
            SemanticError::ContinueOutsideLoop(line) => (
                format!("Line {line}"),
                "'skip' can only be used inside a loop.".to_string(),
                "Move this line inside a `For each` or `While` block.".to_string(),
            ),
            SemanticError::NeedsWatched(name, line) => (
                format!("Line {line}"),
                format!("'{name}' is worked out from a watched decimal, so it has to be watched too."),
                format!("Write `let {name} be a watched decimal with value ...`."),
            ),
            SemanticError::NotWatched(what, found, line) => (
                format!("Line {line}"),
                format!("Only watched decimals have {what}, but this is a {found}."),
                "Make it with `let ... be a watched decimal with value ...`.".to_string(),
            ),
            SemanticError::NoWatchedProperty(property, line) => (
                format!("Line {line}"),
                format!("A watched decimal doesn't have a '{property}'."),
                "Ask for `the value of ...` or `the gradient of ...`.".to_string(),
            ),
            other => {
                let msg = format!("{other}");
                return format!(
                    "Oops! Something went wrong.\n  {msg}\n  Try: Check the line carefully."
                );
            }
        };
        format!("Oops! {line_part} has a problem.\n  {description}\n  Try: {try_hint}")
    }
}

/// Function signature for the symbol table
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub parameters: Vec<(String, Type)>,
    pub return_type: Type,
}

/// Class signature for the symbol table
#[derive(Debug, Clone)]
pub struct ClassSignature {
    pub properties: HashMap<String, Type>,
    pub methods: HashMap<String, FunctionSignature>,
    pub constructor_params: Vec<(String, Type)>,
}

/// Symbol table entry
#[derive(Debug, Clone)]
pub struct Symbol {
    pub symbol_type: Type,
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
    /// Current function name (for beginner inference)
    current_function_name: Option<String>,
    /// Builtin function names that allow Int → Float auto-promotion
    builtin_int_promotable: HashSet<String>,
    /// Current line being analyzed (for error reporting)
    current_line: usize,
    /// Whether we're currently inside a loop
    in_loop: bool,
    /// Collected errors
    pub errors: Vec<SemanticError>,
    /// Names of beginner (Inferred-param) functions
    beginner_functions: HashSet<String>,
    /// Inferred param types per beginner function (index = param position)
    inferred_param_types: HashMap<String, Vec<Option<Type>>>,
    /// Inferred return type per beginner function
    inferred_return_types: HashMap<String, Option<Type>>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut analyzer = Self {
            scopes: vec![HashMap::new()],
            functions: HashMap::new(),
            classes: HashMap::new(),
            current_class: None,
            current_function_return_type: None,
            current_function_name: None,
            builtin_int_promotable: HashSet::new(),
            current_line: 0,
            in_loop: false,
            errors: Vec::new(),
            beginner_functions: HashSet::new(),
            inferred_param_types: HashMap::new(),
            inferred_return_types: HashMap::new(),
        };
        analyzer.register_builtin_functions();
        analyzer
    }

    fn register_builtin_functions(&mut self) {
        for builtin in stdlib::get_all_builtins() {
            let sig = FunctionSignature {
                parameters: builtin
                    .parameters
                    .iter()
                    .map(|(name, typ)| (name.to_string(), typ.clone()))
                    .collect(),
                return_type: builtin.return_type.clone(),
            };
            for name in &builtin.names {
                self.functions.insert(name.to_string(), sig.clone());
                if builtin.accepts_int_as_float {
                    self.builtin_int_promotable.insert(name.to_string());
                }
            }
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
            return Err(SemanticError::AlreadyDeclared(
                name.to_string(),
                self.current_line,
            ));
        }
        scope.insert(
            name.to_string(),
            Symbol {
                symbol_type: var_type,
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
        for (i, stmt) in program.statements.iter().enumerate() {
            if let Some(&line) = program.statement_lines.get(i) {
                self.current_line = line;
            }
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
            return Err(SemanticError::FunctionAlreadyDefined(
                func.name.clone(),
                self.current_line,
            ));
        }
        for param in &func.parameters {
            self.check_watched_placement(&param.param_type)?;
        }
        self.check_watched_placement(&func.return_type)?;
        if func
            .parameters
            .iter()
            .any(|p| p.param_type == Type::Inferred)
            || func.return_type == Type::Inferred
        {
            self.beginner_functions.insert(func.name.clone());
            self.inferred_param_types
                .insert(func.name.clone(), vec![None; func.parameters.len()]);
            self.inferred_return_types.insert(func.name.clone(), None);
        }
        self.functions.insert(
            func.name.clone(),
            FunctionSignature {
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
        self.current_function_name = Some(func.name.clone());
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
            return Err(SemanticError::ClassAlreadyDefined(
                class.name.clone(),
                self.current_line,
            ));
        }

        // Collect parent properties/methods if inheriting
        let mut properties = HashMap::new();
        let mut methods = HashMap::new();

        if let Some(parent_name) = &class.parent {
            if let Some(parent_sig) = self.classes.get(parent_name).cloned() {
                properties.extend(parent_sig.properties);
                methods.extend(parent_sig.methods);
            } else {
                return Err(SemanticError::UndefinedClass(
                    parent_name.clone(),
                    self.current_line,
                ));
            }
        }

        // Add own properties (may override parent's)
        for prop in &class.properties {
            if is_managed(&prop.prop_type) {
                return Err(SemanticError::WatchedNotAllowed(
                    prop.prop_type.clone(),
                    "kinds".to_string(),
                    self.current_line,
                ));
            }
            self.check_watched_placement(&prop.prop_type)?;
            properties.insert(prop.name.clone(), prop.prop_type.clone());
        }

        // Add own methods (may override parent's)
        for method in &class.methods {
            methods.insert(
                method.name.clone(),
                FunctionSignature {
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
                        symbol_type: param.param_type.clone(),
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
                        symbol_type: param.param_type.clone(),
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
                self.check_watched_placement(var_type)?;
                let value_type = self.analyze_expression(value)?;
                self.check_not_losing_watch(name, var_type, &value_type)?;
                self.check_type_compatible(var_type, &value_type)?;
                self.declare_variable(name, var_type.clone())?;
            }

            Statement::Assignment { name, value } => {
                let symbol = self.lookup_variable(name).ok_or_else(|| {
                    SemanticError::UndefinedVariable(name.clone(), self.current_line)
                })?;
                let expected_type = symbol.symbol_type.clone();
                let value_type = self.analyze_expression(value)?;
                self.check_not_losing_watch(name, &expected_type, &value_type)?;
                self.check_type_compatible(&expected_type, &value_type)?;
            }

            Statement::CompoundAssignment { name, op, value } => {
                let symbol = self.lookup_variable(name).ok_or_else(|| {
                    SemanticError::UndefinedVariable(name.clone(), self.current_line)
                })?;
                let var_type = symbol.symbol_type.clone();
                let value_type = self.analyze_expression(value)?;
                let result_type = self.check_binary_op_types(op, &var_type, &value_type)?;
                self.check_not_losing_watch(name, &var_type, &result_type)?;
            }

            Statement::IndexAssignment {
                collection,
                index,
                value,
            } => {
                let symbol = self.lookup_variable(collection).ok_or_else(|| {
                    SemanticError::UndefinedVariable(collection.clone(), self.current_line)
                })?;
                let (expected_index_type, element_type) = match &symbol.symbol_type {
                    Type::List(inner) => (Type::Int, (**inner).clone()),
                    Type::Dict(key, value) => ((**key).clone(), (**value).clone()),
                    other => {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::List(Box::new(Type::Inferred)),
                            found: other.clone(),
                            line: self.current_line,
                        });
                    }
                };
                let index_type = self.analyze_expression(index)?;
                if index_type != expected_index_type {
                    return Err(SemanticError::TypeMismatch {
                        expected: expected_index_type,
                        found: index_type,
                        line: self.current_line,
                    });
                }
                let value_type = self.analyze_expression(value)?;
                self.check_type_compatible(&element_type, &value_type)?;
            }

            Statement::Output(expr) => {
                self.analyze_expression(expr)?;
            }

            Statement::If {
                condition,
                then_block,
                else_ifs,
                else_block,
            } => {
                let cond_type = self.analyze_expression(condition)?;
                if cond_type != Type::Bool {
                    return Err(SemanticError::TypeMismatch {
                        expected: Type::Bool,
                        found: cond_type,
                        line: self.current_line,
                    });
                }

                self.push_scope();
                for stmt in then_block {
                    self.analyze_statement(stmt)?;
                }
                self.pop_scope();

                // Analyze else-if chains
                for (elif_cond, elif_block) in else_ifs {
                    let elif_type = self.analyze_expression(elif_cond)?;
                    if elif_type != Type::Bool {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::Bool,
                            found: elif_type,
                            line: self.current_line,
                        });
                    }
                    self.push_scope();
                    for stmt in elif_block {
                        self.analyze_statement(stmt)?;
                    }
                    self.pop_scope();
                }

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
                        line: self.current_line,
                    });
                }

                self.in_loop = true;
                self.push_scope();
                for stmt in body {
                    self.analyze_statement(stmt)?;
                }
                self.pop_scope();
                self.in_loop = false;
            }

            Statement::For {
                variable,
                start,
                end,
                body,
            } => {
                // Check that start and end are integers
                let start_type = self.analyze_expression(start)?;
                if start_type != Type::Int {
                    return Err(SemanticError::TypeMismatch {
                        expected: Type::Int,
                        found: start_type,
                        line: self.current_line,
                    });
                }

                let end_type = self.analyze_expression(end)?;
                if end_type != Type::Int {
                    return Err(SemanticError::TypeMismatch {
                        expected: Type::Int,
                        found: end_type,
                        line: self.current_line,
                    });
                }

                self.in_loop = true;
                self.push_scope();
                // Declare the loop variable in the new scope
                self.declare_variable(variable, Type::Int)?;
                for stmt in body {
                    self.analyze_statement(stmt)?;
                }
                self.pop_scope();
                self.in_loop = false;
            }

            Statement::Break => {
                if !self.in_loop {
                    return Err(SemanticError::BreakOutsideLoop(self.current_line));
                }
            }

            Statement::Continue => {
                if !self.in_loop {
                    return Err(SemanticError::ContinueOutsideLoop(self.current_line));
                }
            }

            Statement::ExprStatement(expr) => {
                self.analyze_expression(expr)?;
            }

            Statement::Return(expr) => {
                if self.current_function_return_type.is_none() {
                    return Err(SemanticError::ReturnOutsideFunction(self.current_line));
                }
                if let Some(return_expr) = expr {
                    let return_type = self.analyze_expression(return_expr)?;
                    let expected = self.current_function_return_type.clone().unwrap();
                    if expected == Type::Inferred {
                        if let Some(fn_name) = self.find_current_beginner_function() {
                            let entry = self.inferred_return_types.get_mut(&fn_name).unwrap();
                            *entry = Some(return_type);
                        }
                    } else if expected != Type::Void {
                        self.check_type_compatible(&expected, &return_type)?;
                    }
                }
            }

            Statement::PropertyAssignment {
                object,
                property,
                value,
            } => {
                let symbol = self.lookup_variable(object).ok_or_else(|| {
                    SemanticError::UndefinedVariable(object.clone(), self.current_line)
                })?;
                let class_name = match &symbol.symbol_type {
                    Type::Class(name) => name.clone(),
                    other => {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::Class("any".to_string()),
                            found: other.clone(),
                            line: self.current_line,
                        });
                    }
                };
                let class_sig = self.classes.get(&class_name).ok_or_else(|| {
                    SemanticError::UndefinedClass(class_name.clone(), self.current_line)
                })?;
                let prop_type = class_sig
                    .properties
                    .get(property)
                    .ok_or_else(|| {
                        SemanticError::UndefinedProperty(
                            property.clone(),
                            class_name.clone(),
                            self.current_line,
                        )
                    })?
                    .clone();
                let value_type = self.analyze_expression(value)?;
                self.check_type_compatible(&prop_type, &value_type)?;
            }

            Statement::FindGradients(expr) => {
                self.expect_watched(expr, "gradients")?;
            }

            Statement::ShowGraph(expr) => {
                self.expect_watched(expr, "a graph")?;
            }

            Statement::Plot {
                data,
                against,
                chart_type: _,
                title: _,
                output_file: _,
            } => {
                // Validate that data is an array
                let data_type = self.analyze_expression(data)?;
                match &data_type {
                    Type::List(_) => {}
                    _ => {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::List(Box::new(Type::Float)),
                            found: data_type,
                            line: self.current_line,
                        });
                    }
                }

                // If against is provided, validate it's also an array
                if let Some(against_expr) = against {
                    let against_type = self.analyze_expression(against_expr)?;
                    match &against_type {
                        Type::List(_) => {}
                        _ => {
                            return Err(SemanticError::TypeMismatch {
                                expected: Type::List(Box::new(Type::Float)),
                                found: against_type,
                                line: self.current_line,
                            });
                        }
                    }
                }
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
                let symbol = self.lookup_variable(name).ok_or_else(|| {
                    SemanticError::UndefinedVariable(name.clone(), self.current_line)
                })?;
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
                        if matches!(
                            operand_type,
                            Type::Int | Type::Float | Type::Watched | Type::Tensor
                        ) {
                            Ok(operand_type)
                        } else {
                            Err(SemanticError::TypeMismatch {
                                expected: Type::Int,
                                found: operand_type,
                                line: self.current_line,
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
                                line: self.current_line,
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
                                line: self.current_line,
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
                    Type::Tensor if idx_type == Type::Int => Ok(Type::Tensor),
                    Type::List(inner) => {
                        if idx_type != Type::Int {
                            return Err(SemanticError::TypeMismatch {
                                expected: Type::Int,
                                found: idx_type,
                                line: self.current_line,
                            });
                        }
                        Ok(*inner)
                    }
                    Type::Text => {
                        if idx_type != Type::Int {
                            return Err(SemanticError::TypeMismatch {
                                expected: Type::Int,
                                found: idx_type,
                                line: self.current_line,
                            });
                        }
                        Ok(Type::Text)
                    }
                    Type::Dict(key_type, value_type) => {
                        if idx_type != *key_type {
                            return Err(SemanticError::TypeMismatch {
                                expected: *key_type,
                                found: idx_type,
                                line: self.current_line,
                            });
                        }
                        Ok(*value_type)
                    }
                    _ => Err(SemanticError::TypeMismatch {
                        expected: Type::List(Box::new(Type::Int)),
                        found: coll_type,
                        line: self.current_line,
                    }),
                }
            }

            Expr::FunctionCall { name, arguments } if name == "append" || name == "push" => {
                if arguments.len() != 2 {
                    return Err(SemanticError::ArgumentCountMismatch(
                        name.clone(),
                        2,
                        arguments.len(),
                        self.current_line,
                    ));
                }
                let list_type = self.analyze_expression(&arguments[0])?;
                let elem_type = match list_type {
                    Type::List(inner) => *inner,
                    other => {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::List(Box::new(Type::Int)),
                            found: other,
                            line: self.current_line,
                        });
                    }
                };
                let value_type = self.analyze_expression(&arguments[1])?;
                if value_type != elem_type && !(elem_type == Type::Float && value_type == Type::Int)
                {
                    return Err(SemanticError::TypeMismatch {
                        expected: elem_type,
                        found: value_type,
                        line: self.current_line,
                    });
                }
                Ok(Type::List(Box::new(elem_type)))
            }

            Expr::FunctionCall { name, arguments }
                if self.is_tensor_call(name, arguments)? =>
            {
                self.check_tensor_call(name, arguments)
            }

            Expr::FunctionCall { name, arguments } => {
                if let Some((_, arity)) = stdlib::watched_function(name)
                    && let Some(first) = arguments.first()
                    && self.analyze_expression(first)? == Type::Watched
                {
                    if arguments.len() != arity {
                        return Err(SemanticError::ArgumentCountMismatch(
                            name.clone(),
                            arity,
                            arguments.len(),
                            self.current_line,
                        ));
                    }
                    if let Some(exponent) = arguments.get(1) {
                        let exponent_type = self.analyze_expression(exponent)?;
                        if !matches!(exponent_type, Type::Int | Type::Float) {
                            return Err(SemanticError::TypeMismatch {
                                expected: Type::Float,
                                found: exponent_type,
                                line: self.current_line,
                            });
                        }
                    }
                    return Ok(Type::Watched);
                }

                let sig = self
                    .functions
                    .get(name)
                    .ok_or_else(|| {
                        SemanticError::UndefinedFunction(name.clone(), self.current_line)
                    })?
                    .clone();

                if arguments.len() != sig.parameters.len() {
                    return Err(SemanticError::ArgumentCountMismatch(
                        name.clone(),
                        sig.parameters.len(),
                        arguments.len(),
                        self.current_line,
                    ));
                }

                if self.beginner_functions.contains(name) {
                    let mut arg_types = Vec::new();
                    for arg in arguments.iter() {
                        arg_types.push(self.analyze_expression(arg)?);
                    }
                    let inferred = self.inferred_param_types.get_mut(name).unwrap();
                    for (i, arg_type) in arg_types.into_iter().enumerate() {
                        match &inferred[i] {
                            None => inferred[i] = Some(arg_type),
                            Some(existing) if existing == &arg_type => {}
                            Some(existing) => {
                                let expected = existing.clone();
                                return Err(SemanticError::TypeMismatch {
                                    expected,
                                    found: arg_type,
                                    line: self.current_line,
                                });
                            }
                        }
                    }
                    let ret = self.inferred_return_types.get(name).unwrap().clone();
                    return Ok(ret.unwrap_or(Type::Void));
                }

                for (arg, (_param_name, param_type)) in arguments.iter().zip(sig.parameters.iter())
                {
                    let arg_type = self.analyze_expression(arg)?;
                    if param_type == &Type::Float
                        && arg_type == Type::Int
                        && self.builtin_int_promotable.contains(name)
                    {
                        continue;
                    }
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
                    .ok_or_else(|| {
                        SemanticError::UndefinedClass(class_name.clone(), self.current_line)
                    })?
                    .clone();

                if arguments.len() != class_sig.constructor_params.len() {
                    return Err(SemanticError::ArgumentCountMismatch(
                        class_name.clone(),
                        class_sig.constructor_params.len(),
                        arguments.len(),
                        self.current_line,
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
                            line: self.current_line,
                        });
                    }
                };

                let class_sig = self
                    .classes
                    .get(&class_name)
                    .ok_or_else(|| {
                        SemanticError::UndefinedClass(class_name.clone(), self.current_line)
                    })?
                    .clone();

                let method_sig = class_sig
                    .methods
                    .get(method)
                    .ok_or_else(|| {
                        SemanticError::UndefinedMethod(
                            method.clone(),
                            class_name.clone(),
                            self.current_line,
                        )
                    })?
                    .clone();

                if arguments.len() != method_sig.parameters.len() {
                    return Err(SemanticError::ArgumentCountMismatch(
                        format!("{}.{}", class_name, method),
                        method_sig.parameters.len(),
                        arguments.len(),
                        self.current_line,
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
                let asks_for_gradient = property == "gradient" || property == "value";
                let class_name = match &object_type {
                    Type::Tensor => {
                        return match property.as_str() {
                            "shape" => Ok(Type::List(Box::new(Type::Int))),
                            "value" => Ok(Type::Float),
                            "sum" | "mean" | "transpose" => Ok(Type::Tensor),
                            _ => Err(SemanticError::NoTensorProperty(
                                property.clone(),
                                self.current_line,
                            )),
                        };
                    }
                    Type::Watched if asks_for_gradient => return Ok(Type::Float),
                    Type::Watched => {
                        return Err(SemanticError::NoWatchedProperty(
                            property.clone(),
                            self.current_line,
                        ));
                    }
                    Type::Int | Type::Float if property == "gradient" => {
                        return Err(SemanticError::NotWatched(
                            "a gradient".to_string(),
                            object_type.clone(),
                            self.current_line,
                        ));
                    }
                    Type::Class(name) => name.clone(),
                    other => {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::Class("any".to_string()),
                            found: other.clone(),
                            line: self.current_line,
                        });
                    }
                };

                let class_sig = self.classes.get(&class_name).ok_or_else(|| {
                    SemanticError::UndefinedClass(class_name.clone(), self.current_line)
                })?;

                let prop_type = class_sig
                    .properties
                    .get(property)
                    .ok_or_else(|| {
                        SemanticError::UndefinedProperty(
                            property.clone(),
                            class_name.clone(),
                            self.current_line,
                        )
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
        if *left == Type::Inferred || *right == Type::Inferred {
            return Ok(Type::Inferred);
        }
        if *left == Type::Tensor || *right == Type::Tensor {
            let fits = |t: &Type| *t == Type::Tensor || becomes_tensor(t);
            return match op {
                BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide
                    if fits(left) && fits(right) =>
                {
                    Ok(Type::Tensor)
                }
                _ => Err(SemanticError::InvalidOperator {
                    op: op.clone(),
                    left: left.clone(),
                    right: right.clone(),
                    line: self.current_line,
                }),
            };
        }
        let numeric = |t: &Type| matches!(t, Type::Int | Type::Float | Type::Watched);
        if (*left == Type::Watched || *right == Type::Watched) && numeric(left) && numeric(right) {
            match op {
                BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                    return Ok(Type::Watched);
                }
                BinaryOp::Equal
                | BinaryOp::NotEqual
                | BinaryOp::Greater
                | BinaryOp::Less
                | BinaryOp::GreaterEq
                | BinaryOp::LessEq => return Ok(Type::Bool),
                _ => {}
            }
        }
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
                        line: self.current_line,
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
                        line: self.current_line,
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
                        line: self.current_line,
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
                        line: self.current_line,
                    })
                }
            }
        }
    }

    fn check_type_compatible(&self, expected: &Type, found: &Type) -> Result<(), SemanticError> {
        if expected == found
            || *expected == Type::Inferred
            || *found == Type::Inferred
            || (*expected == Type::Watched && matches!(found, Type::Int | Type::Float))
            || (*expected == Type::Tensor && becomes_tensor(found))
        {
            Ok(())
        } else if let (Type::Dict(ek, ev), Type::Dict(fk, fv)) = (expected, found) {
            self.check_type_compatible(ek, fk)?;
            self.check_type_compatible(ev, fv)
        } else {
            Err(SemanticError::TypeMismatch {
                expected: expected.clone(),
                found: found.clone(),
                line: self.current_line,
            })
        }
    }

    fn is_tensor_call(&mut self, name: &str, arguments: &[Expr]) -> Result<bool, SemanticError> {
        let Some((_, tensor_only)) = stdlib::tensor_function(name) else {
            return Ok(false);
        };
        if tensor_only {
            return Ok(!self.functions.contains_key(name));
        }
        match arguments.first() {
            Some(first) => Ok(self.analyze_expression(first)? == Type::Tensor),
            None => Ok(false),
        }
    }

    fn check_tensor_call(&mut self, name: &str, arguments: &[Expr]) -> Result<Type, SemanticError> {
        use stdlib::TensorCall::*;
        let (call, _) = stdlib::tensor_function(name).unwrap();
        let (tensors, numbers, any_count) = match call {
            Filled(_) => (0, 1, true),
            Reshape => (1, 1, true),
            Unary(_) => (1, 0, false),
            Power | Along(_) => (1, 1, false),
            Binary(_) => (2, 0, false),
        };
        let needed = tensors + numbers;
        if arguments.len() < needed || (!any_count && arguments.len() > needed) {
            return Err(SemanticError::ArgumentCountMismatch(
                name.to_string(),
                needed,
                arguments.len(),
                self.current_line,
            ));
        }
        for (i, arg) in arguments.iter().enumerate() {
            let found = self.analyze_expression(arg)?;
            let expected = if i < tensors {
                Type::Tensor
            } else if call == Power {
                Type::Float
            } else {
                Type::Int
            };
            let fits = match expected {
                Type::Tensor => found == Type::Tensor || becomes_tensor(&found),
                Type::Float => matches!(found, Type::Int | Type::Float),
                _ => found == Type::Int,
            };
            if !fits && found != Type::Inferred {
                return Err(SemanticError::TypeMismatch {
                    expected,
                    found,
                    line: self.current_line,
                });
            }
        }
        Ok(Type::Tensor)
    }

    fn check_not_losing_watch(
        &self,
        name: &str,
        target: &Type,
        value: &Type,
    ) -> Result<(), SemanticError> {
        if *value == Type::Watched && matches!(target, Type::Int | Type::Float) {
            Err(SemanticError::NeedsWatched(name.to_string(), self.current_line))
        } else if *value == Type::Tensor && *target != Type::Tensor && *target != Type::Inferred {
            Err(SemanticError::TypeMismatch {
                expected: target.clone(),
                found: Type::Tensor,
                line: self.current_line,
            })
        } else {
            Ok(())
        }
    }

    fn check_watched_placement(&self, typ: &Type) -> Result<(), SemanticError> {
        let place: (&str, &Box<Type>) = match typ {
            Type::List(inner) | Type::Set(inner) if is_managed(inner) => ("lists", inner),
            Type::Dict(_, inner) if is_managed(inner) => ("dictionaries", inner),
            _ => return Ok(()),
        };
        Err(SemanticError::WatchedNotAllowed(
            (**place.1).clone(),
            place.0.to_string(),
            self.current_line,
        ))
    }

    fn expect_watched(&mut self, expr: &Expr, what: &str) -> Result<(), SemanticError> {
        let found = self.analyze_expression(expr)?;
        if found == Type::Watched || found == Type::Inferred {
            Ok(())
        } else {
            Err(SemanticError::NotWatched(what.to_string(), found, self.current_line))
        }
    }

    fn find_current_beginner_function(&self) -> Option<String> {
        let name = self.current_function_name.as_ref()?;
        if self.beginner_functions.contains(name) {
            Some(name.clone())
        } else {
            None
        }
    }

    pub fn patch_program_types(&self, program: &mut Program) {
        for func in &mut program.functions {
            if !self.beginner_functions.contains(&func.name) {
                continue;
            }
            if let Some(inferred_params) = self.inferred_param_types.get(&func.name) {
                for (param, inferred) in func.parameters.iter_mut().zip(inferred_params.iter()) {
                    if param.param_type == Type::Inferred {
                        param.param_type = inferred.clone().unwrap_or(Type::Int);
                    }
                }
            }
            if func.return_type == Type::Inferred {
                func.return_type = self
                    .inferred_return_types
                    .get(&func.name)
                    .and_then(|t| t.clone())
                    .unwrap_or(Type::Void);
            }
        }
    }

    fn check_conversion(&self, from: &Type, to: &Type) -> Result<(), SemanticError> {
        let valid = matches!(
            (from, to),
            (Type::Watched, Type::Float)
                | (Type::Tensor, Type::Float)
                | (Type::Int, Type::Float)
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
                line: self.current_line,
            })
        }
    }
}

/// Watched decimals and tensors live in the runtime and are counted, so they
/// can't yet be stored where codegen doesn't track them.
pub fn is_managed(typ: &Type) -> bool {
    matches!(typ, Type::Watched | Type::Tensor)
}

/// Numbers and (nested) lists of numbers turn into a tensor automatically.
pub fn becomes_tensor(typ: &Type) -> bool {
    match typ {
        Type::Int | Type::Float => true,
        Type::List(inner) => becomes_tensor(inner),
        _ => false,
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

    #[test]
    fn test_inferred_types_resolved_from_call_site() {
        let src = "use beginner.\nTo double x:\n    Give back x.\nEnd.\noutput double of 5.";
        let mut program = Parser::parse(src).unwrap();
        let mut analyzer = SemanticAnalyzer::new();
        assert!(analyzer.analyze(&program).is_ok());
        analyzer.patch_program_types(&mut program);
        let f = &program.functions[0];
        assert_eq!(f.parameters[0].param_type, Type::Int);
    }

    #[test]
    fn test_inferred_type_conflict_produces_error() {
        let src = concat!(
            "use beginner.\n",
            "To greet x:\n    output x.\nEnd.\n",
            "Call greet with 5.\n",
            "Call greet with \"hello\".\n"
        );
        let program = Parser::parse(src).unwrap();
        let mut analyzer = SemanticAnalyzer::new();
        assert!(analyzer.analyze(&program).is_err());
    }

    fn first_error(source: &str) -> SemanticError {
        analyze(source).unwrap_err().remove(0)
    }

    #[test]
    fn watched_decimals_mix_with_plain_numbers() {
        let src = concat!(
            "let w be a watched decimal with value 1.\n",
            "let loss be a watched decimal with value (w * 3.0 - 6) * (w * 3.0 - 6).\n",
            "Find the gradients of loss.\n",
            "let slope be a decimal with value the gradient of w.\n",
            "Subtract 0.1 * slope from w.\n",
            "let squashed be a watched decimal with value the result of sigmoid with loss.\n",
            "If loss is less than 1.0,\n    output the value of loss.\nEnd.\n"
        );
        assert!(analyze(src).is_ok());
    }

    #[test]
    fn storing_a_watched_result_in_a_decimal_is_explained() {
        let error = first_error(
            "let w be a watched decimal with value 0.5.\nlet loss be a decimal with value w * 3.0.",
        );
        assert!(matches!(error, SemanticError::NeedsWatched(ref name, 2) if name == "loss"));
    }

    #[test]
    fn plain_decimals_have_no_gradient() {
        let error = first_error("let x be a decimal with value 0.5.\noutput the gradient of x.");
        assert!(matches!(error, SemanticError::NotWatched(_, Type::Float, 2)));
        let error = first_error("let x be a decimal with value 0.5.\nFind the gradients of x.");
        assert!(matches!(error, SemanticError::NotWatched(_, Type::Float, 2)));
    }

    #[test]
    fn watched_decimals_stay_out_of_lists_and_kinds() {
        let error = first_error("let ws be a list of watched decimal with value [1.0].");
        assert!(matches!(error, SemanticError::WatchedNotAllowed(_, _, 1)));
        let error = first_error(
            "Define a kind called Counter with the following:\n    Property weight is a watched decimal.\nEnd kind.",
        );
        assert!(matches!(error, SemanticError::WatchedNotAllowed(_, _, _)));
    }

    #[test]
    fn lists_and_numbers_become_tensors() {
        let src = concat!(
            "let inputs be a tensor with value [[0.0, 1.0], [1.0, 0.0]].\n",
            "let weights be a tensor with value the result of randomTensor with 2 and 3.\n",
            "let biases be a tensor with value [1, 2, 3].\n",
            "let hidden be a tensor with value the result of relu with (the result of matmul with inputs and weights) + biases.\n",
            "let scaled be a tensor with value hidden * 2 - 1.0.\n",
            "let corner be a tensor with value scaled[0][1].\n",
            "let total be a decimal with value the value of corner.\n",
            "let rows be a list of standard number with value the shape of hidden.\n",
            "let average be a tensor with value the mean of hidden.\n"
        );
        assert!(analyze(src).is_ok(), "{:?}", analyze(src));
    }

    #[test]
    fn tensor_mistakes_are_caught_before_running() {
        let error = first_error("let t be a tensor with value [1.0].\nlet x be a decimal with value t * 2.");
        assert!(matches!(error, SemanticError::TypeMismatch { found: Type::Tensor, .. }));
        let error = first_error("let t be a tensor with value [1.0].\noutput the size of t.");
        assert!(matches!(error, SemanticError::NoTensorProperty(_, 2)));
        let error = first_error("let t be a tensor with value [1.0].\noutput t is less than 2.");
        assert!(matches!(error, SemanticError::InvalidOperator { .. }));
        let error = first_error("let t be a tensor with value the result of zeroTensor with 2.5.");
        assert!(matches!(error, SemanticError::TypeMismatch { expected: Type::Int, .. }));
    }
}
