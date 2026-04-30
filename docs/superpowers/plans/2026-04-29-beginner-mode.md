# Beginner Mode Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `use beginner.` file-header mode to eng-lish that enables simplified function syntax and kid-friendly error messages for ages 10–13.

**Architecture:** A `beginner_mode: bool` flag threads through Parser → AST → SemanticAnalyzer → main. The parser recognizes two new syntax forms when the flag is set. The semantic analyzer resolves `Type::Inferred` parameter types from call sites and patches the AST before codegen. Error output in main switches to `Oops!` format when the source file starts with `use beginner.`.

**Tech Stack:** Rust, logos (lexer), inkwell/LLVM (codegen), cargo test (unit tests in `#[cfg(test)]` modules in each source file)

---

## File Map

| File | Change |
|---|---|
| `src/ast.rs` | Add `Type::Inferred`, add `beginner_mode: bool` to `Program` |
| `src/parser.rs` | Add `beginner_mode` to `Parser`; parse `use beginner.`; parse beginner func defs and `name of args` calls |
| `src/semantic.rs` | Resolve `Type::Inferred` params from call sites; expose `patch_program_types()`; add beginner error strings |
| `src/main.rs` | Pre-detect beginner mode; call `patch_program_types`; route errors to beginner formatter |
| `examples/beginner_squares.eng` | New example beginner program |
| `examples/beginner_greeting.eng` | New example beginner program with text param |

---

## Task 1: Add `Type::Inferred` and `beginner_mode` to AST

**Files:**
- Modify: `src/ast.rs`

- [ ] **Step 1: Add `Type::Inferred` variant to the `Type` enum**

In `src/ast.rs`, add `Inferred` to the `Type` enum and its `Display` impl:

```rust
pub enum Type {
    Text,
    Int,
    Float,
    Bool,
    List(Box<Type>),
    Dict(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Set(Box<Type>),
    Void,
    Class(String),
    Inferred,   // add this line
}
```

In the `Display` impl, add:
```rust
Type::Inferred => write!(f, "unknown"),
```

- [ ] **Step 2: Add `beginner_mode` to `Program`**

In `src/ast.rs`, update the `Program` struct:

```rust
pub struct Program {
    pub imports: Vec<String>,
    pub classes: Vec<ClassDef>,
    pub functions: Vec<FunctionDef>,
    pub statements: Vec<Statement>,
    pub statement_lines: Vec<usize>,
    pub beginner_mode: bool,   // add this line
}
```

Update `Program::new()`:
```rust
pub fn new() -> Self {
    Self {
        imports: Vec::new(),
        classes: Vec::new(),
        functions: Vec::new(),
        statements: Vec::new(),
        statement_lines: Vec::new(),
        beginner_mode: false,
    }
}
```

- [ ] **Step 3: Run `cargo build` to verify it compiles**

```bash
cargo build 2>&1 | head -30
```

Expected: compile errors only for non-exhaustive `match` on `Type::Inferred` in other files. That's fine — we'll fix them as we go.

- [ ] **Step 4: Commit**

```bash
git add src/ast.rs
git commit -m "Add Type::Inferred and beginner_mode to AST"
```

---

## Task 2: Parse `use beginner.` header

**Files:**
- Modify: `src/parser.rs`

- [ ] **Step 1: Write the failing test**

At the bottom of `src/parser.rs` in the `#[cfg(test)]` module, add:

```rust
#[test]
fn test_beginner_mode_header() {
    let program = Parser::parse("use beginner.\noutput \"hello\".").unwrap();
    assert!(program.beginner_mode);
}

#[test]
fn test_non_beginner_mode() {
    let program = Parser::parse("output \"hello\".").unwrap();
    assert!(!program.beginner_mode);
}
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
cargo test test_beginner_mode_header 2>&1 | tail -20
```

Expected: FAIL — `program` has no `beginner_mode` field yet on the parser side.

- [ ] **Step 3: Add `beginner_mode` to `Parser` struct**

In `src/parser.rs`, update the `Parser` struct:

```rust
pub struct Parser {
    tokens: Vec<SpannedToken>,
    current: usize,
    beginner_mode: bool,
}
```

Update `Parser::new()`:
```rust
pub fn new(tokens: Vec<SpannedToken>) -> Self {
    Self { tokens, current: 0, beginner_mode: false }
}
```

- [ ] **Step 4: Detect `use beginner.` in `parse_program()`**

In `parse_program()`, the `use` branch currently matches `StringLiteral`. Add a branch for `Identifier("beginner")` before the string literal branch:

```rust
Some(Token::Use) => {
    self.advance();
    match self.current().cloned() {
        Some(Token::Identifier(ref name)) if name == "beginner" => {
            self.advance();
            self.expect(Token::Period)?;
            self.beginner_mode = true;
        }
        Some(Token::StringLiteral(name)) => {
            self.advance();
            self.expect(Token::Period)?;
            imports.push(name);
        }
        Some(tok) => {
            return Err(ParseError::UnexpectedToken {
                expected: "a package name in quotes".to_string(),
                found: tok,
                line: self.current_line(),
            });
        }
        None => return Err(ParseError::UnexpectedEof),
    }
}
```

- [ ] **Step 5: Propagate `beginner_mode` to the returned `Program`**

At the end of `parse_program()`, update the `Ok(...)` return:

```rust
Ok(Program {
    imports,
    classes,
    functions,
    statements,
    statement_lines,
    beginner_mode: self.beginner_mode,
})
```

- [ ] **Step 6: Run tests to verify they pass**

```bash
cargo test test_beginner_mode_header test_non_beginner_mode 2>&1 | tail -10
```

Expected: both PASS.

- [ ] **Step 7: Commit**

```bash
git add src/parser.rs
git commit -m "Parse use beginner. header and set beginner_mode on Program"
```

---

## Task 3: Parse beginner function definitions

**Files:**
- Modify: `src/parser.rs`

Beginner function signatures: `To name param and param:` (untyped params → `Type::Inferred`, implicit `Type::Inferred` return type).

- [ ] **Step 1: Write the failing tests**

In the `#[cfg(test)]` block of `src/parser.rs`:

```rust
#[test]
fn test_beginner_func_def_one_param() {
    let src = "use beginner.\nTo double x:\n    Give back x.\nEnd.";
    let program = Parser::parse(src).unwrap();
    assert_eq!(program.functions.len(), 1);
    let f = &program.functions[0];
    assert_eq!(f.name, "double");
    assert_eq!(f.parameters.len(), 1);
    assert_eq!(f.parameters[0].name, "x");
    assert_eq!(f.parameters[0].param_type, Type::Inferred);
    assert_eq!(f.return_type, Type::Inferred);
}

#[test]
fn test_beginner_func_def_two_params() {
    let src = "use beginner.\nTo add x and y:\n    Give back x.\nEnd.";
    let program = Parser::parse(src).unwrap();
    let f = &program.functions[0];
    assert_eq!(f.parameters.len(), 2);
    assert_eq!(f.parameters[0].name, "x");
    assert_eq!(f.parameters[1].name, "y");
}

#[test]
fn test_beginner_func_def_no_params() {
    let src = "use beginner.\nTo sayHello:\n    output \"hi\".\nEnd.";
    let program = Parser::parse(src).unwrap();
    let f = &program.functions[0];
    assert_eq!(f.name, "sayHello");
    assert_eq!(f.parameters.len(), 0);
}
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
cargo test test_beginner_func_def 2>&1 | tail -20
```

Expected: FAIL.

- [ ] **Step 3: Update `is_function_def()` to detect beginner param signatures**

Replace the `is_function_def` method:

```rust
fn is_function_def(&self) -> bool {
    if let Some(Token::To) = self.tokens.get(self.current).map(|t| &t.token) {
        if let Some(Token::Identifier(_)) = self.tokens.get(self.current + 1).map(|t| &t.token) {
            if let Some(next) = self.tokens.get(self.current + 2).map(|t| &t.token) {
                if matches!(next, Token::With | Token::Returning | Token::Colon) {
                    return true;
                }
                // Beginner mode: "To name ident:" (one or more untyped params)
                if self.beginner_mode {
                    if matches!(next, Token::Identifier(_)) {
                        return true;
                    }
                }
            }
        }
    }
    false
}
```

- [ ] **Step 4: Add `parse_beginner_parameter_list()` method**

Add this method to `impl Parser` (near the existing `parse_parameter_list`):

```rust
fn parse_beginner_parameter_list(&mut self) -> Result<Vec<Parameter>, ParseError> {
    let mut params = Vec::new();
    loop {
        let name = self.expect_identifier()?;
        params.push(Parameter { name, param_type: Type::Inferred });
        if !self.match_token(&Token::And) {
            break;
        }
    }
    Ok(params)
}
```

- [ ] **Step 5: Update `parse_function_def()` to use beginner params and return type**

In `parse_function_def()`, replace the parameter parsing block:

```rust
// Replace this existing block:
let parameters = if self.match_token(&Token::With) {
    self.parse_parameter_list()?
} else {
    Vec::new()
};

// With this:
let parameters = if self.match_token(&Token::With) {
    self.parse_parameter_list()?
} else if self.beginner_mode && matches!(self.current(), Some(Token::Identifier(_))) {
    self.parse_beginner_parameter_list()?
} else {
    Vec::new()
};
```

And replace the return type block:

```rust
// Replace this existing block:
let return_type = if self.match_token(&Token::Returning) {
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

// With this:
let return_type = if self.match_token(&Token::Returning) {
    if self.check(&Token::A) || self.check(&Token::An) {
        self.advance();
    }
    if self.match_token(&Token::Nothing) {
        Type::Void
    } else {
        self.parse_type()?
    }
} else if self.beginner_mode {
    Type::Inferred
} else {
    Type::Void
};
```

- [ ] **Step 6: Run tests to verify they pass**

```bash
cargo test test_beginner_func_def 2>&1 | tail -10
```

Expected: all three PASS.

- [ ] **Step 7: Commit**

```bash
git add src/parser.rs
git commit -m "Parse beginner function definitions with untyped params"
```

---

## Task 4: Parse beginner function calls (`name of args`)

**Files:**
- Modify: `src/parser.rs`

In beginner mode, `double of 5` and `add of 3 and 7` are function call expressions. This is parsed in `parse_primary()` when we see `Identifier(name)` followed by `Of`.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn test_beginner_call_single_arg() {
    let src = "use beginner.\noutput double of 5.";
    let program = Parser::parse(src).unwrap();
    match &program.statements[0] {
        Statement::Output(Expr::FunctionCall { name, arguments }) => {
            assert_eq!(name, "double");
            assert_eq!(arguments.len(), 1);
            match &arguments[0] {
                Expr::IntLiteral(5) => {}
                other => panic!("expected IntLiteral(5), got {:?}", other),
            }
        }
        other => panic!("expected Output(FunctionCall), got {:?}", other),
    }
}

#[test]
fn test_beginner_call_two_args() {
    let src = "use beginner.\noutput add of 3 and 7.";
    let program = Parser::parse(src).unwrap();
    match &program.statements[0] {
        Statement::Output(Expr::FunctionCall { name, arguments }) => {
            assert_eq!(name, "add");
            assert_eq!(arguments.len(), 2);
        }
        other => panic!("expected Output(FunctionCall), got {:?}", other),
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
cargo test test_beginner_call 2>&1 | tail -20
```

Expected: FAIL — `double of 5` is not yet parsed as a function call.

- [ ] **Step 3: Add beginner call parsing to `parse_primary()`**

In the `Some(Token::Identifier(name))` branch of `parse_primary()`, after the `Created` check and before the final `Ok(Expr::Identifier(name))`, add:

```rust
// Beginner mode: "name of arg" or "name of arg and arg"
if self.beginner_mode && self.check(&Token::Of) {
    self.advance(); // consume "of"
    let arguments = self.parse_argument_list()?;
    return Ok(Expr::FunctionCall { name, arguments });
}
```

The full updated `Identifier` branch becomes:
```rust
Some(Token::Identifier(name)) => {
    self.advance();
    if self.check(&Token::Created) {
        self.advance();
        self.expect(Token::With)?;
        let arguments = self.parse_argument_list()?;
        return Ok(Expr::NewObject { class_name: name, arguments });
    }
    // Beginner mode: "name of arg" or "name of arg and arg"
    if self.beginner_mode && self.check(&Token::Of) {
        self.advance(); // consume "of"
        let arguments = self.parse_argument_list()?;
        return Ok(Expr::FunctionCall { name, arguments });
    }
    Ok(Expr::Identifier(name))
}
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
cargo test test_beginner_call 2>&1 | tail -10
```

Expected: both PASS.

- [ ] **Step 5: Commit**

```bash
git add src/parser.rs
git commit -m "Parse beginner-mode function calls: name of args"
```

---

## Task 5: Semantic analysis for `Type::Inferred` parameters

**Files:**
- Modify: `src/semantic.rs`

The semantic analyzer needs to:
1. Track which functions have `Inferred` params (beginner functions)
2. Collect the argument types from the first call site for each param
3. Detect conflicts on subsequent call sites
4. Expose `patch_program_types()` to mutate the AST before codegen

- [ ] **Step 1: Write the failing tests**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn test_inferred_types_resolved_from_call_site() {
        let src = "use beginner.\nTo double x:\n    Give back x.\nEnd.\noutput double of 5.";
        let mut program = Parser::parse(src).unwrap();
        let mut analyzer = SemanticAnalyzer::new();
        // Should not error — types resolve from the call site
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
}
```

Add these tests to the existing `#[cfg(test)]` block in `src/semantic.rs`.

- [ ] **Step 2: Run tests to verify they fail**

```bash
cargo test test_inferred_types 2>&1 | tail -20
```

Expected: FAIL — `patch_program_types` doesn't exist yet.

- [ ] **Step 3: Add inference fields to `SemanticAnalyzer`**

In `SemanticAnalyzer` struct, add:

```rust
pub struct SemanticAnalyzer {
    // ... existing fields ...
    /// For beginner functions: inferred param types per function (index = param position)
    inferred_param_types: HashMap<String, Vec<Option<Type>>>,
    /// For beginner functions: inferred return type
    inferred_return_types: HashMap<String, Option<Type>>,
    /// Names of beginner (Inferred-param) functions
    beginner_functions: HashSet<String>,
}
```

Initialize them in `SemanticAnalyzer::new()`:

```rust
inferred_param_types: HashMap::new(),
inferred_return_types: HashMap::new(),
beginner_functions: HashSet::new(),
```

- [ ] **Step 4: Register beginner functions in `register_function()`**

In `register_function()`, after the existing duplicate-check, add:

```rust
// Track beginner functions separately for type inference
if func.parameters.iter().any(|p| p.param_type == Type::Inferred)
    || func.return_type == Type::Inferred
{
    self.beginner_functions.insert(func.name.clone());
    self.inferred_param_types.insert(
        func.name.clone(),
        vec![None; func.parameters.len()],
    );
    self.inferred_return_types.insert(func.name.clone(), None);
}
```

And update the `FunctionSignature` stored in `self.functions` to use `Type::Int` placeholders for `Inferred` params (so count-checks work), and `Type::Void` for return until resolved. The real types will come from `patch_program_types`.

Actually, store the signature as-is with `Type::Inferred` — we'll skip type-checking for `Inferred` params at call sites.

- [ ] **Step 5: Handle `Inferred` at call sites in `analyze_expression()`**

In the `Expr::FunctionCall` branch of `analyze_expression()`, after the argument count check, replace the type-checking loop for beginner functions:

```rust
Expr::FunctionCall { name, arguments } => {
    let sig = self
        .functions
        .get(name)
        .ok_or_else(|| SemanticError::UndefinedFunction(name.clone(), self.current_line))?
        .clone();

    if arguments.len() != sig.parameters.len() {
        return Err(SemanticError::ArgumentCountMismatch(
            name.clone(),
            sig.parameters.len(),
            arguments.len(),
            self.current_line,
        ));
    }

    // For beginner functions: infer/validate param types from this call site
    if self.beginner_functions.contains(name) {
        let inferred = self.inferred_param_types.get_mut(name).unwrap();
        for (i, arg) in arguments.iter().enumerate() {
            let arg_type = self.analyze_expression(arg)?;
            match &inferred[i] {
                None => inferred[i] = Some(arg_type),
                Some(existing) if existing == &arg_type => {}
                Some(existing) => {
                    return Err(SemanticError::TypeMismatch {
                        expected: existing.clone(),
                        found: arg_type,
                        line: self.current_line,
                    });
                }
            }
        }
        // Return type is Inferred until we know it from the function body
        let ret = self.inferred_return_types.get(name).unwrap().clone();
        return Ok(ret.unwrap_or(Type::Void));
    }

    // Standard type checking for non-beginner functions
    for (arg, (_param_name, param_type)) in arguments.iter().zip(sig.parameters.iter()) {
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
```

- [ ] **Step 6: Infer return type from `Give back` in beginner function bodies**

In `analyze_function()`, find the `Return` statement handling (around line 559). After the existing return-type check, add inference for beginner functions:

```rust
Statement::Return(Some(return_expr)) => {
    let return_type = self.analyze_expression(return_expr)?;
    if let Some(current_fn) = &self.current_function_return_type.clone() {
        if current_fn == &Type::Inferred {
            // Infer return type for beginner functions
            if let Some(fn_name) = self.find_current_beginner_function() {
                let entry = self.inferred_return_types.get_mut(&fn_name).unwrap();
                *entry = Some(return_type);
            }
            return Ok(());
        }
        self.check_type_compatible(current_fn, &return_type)?;
    }
}
```

To support `find_current_beginner_function`, add a `current_function_name: Option<String>` field to `SemanticAnalyzer` and set it in `analyze_function()` before processing the body:

```rust
fn analyze_function(&mut self, func: &FunctionDef) -> Result<(), SemanticError> {
    self.current_function_name = Some(func.name.clone());  // add this
    self.current_function_return_type = Some(func.return_type.clone());
    // ... rest of existing code
}
```

Add `current_function_name: Option<String>` to the struct and initialize it to `None`.

Add the helper:
```rust
fn find_current_beginner_function(&self) -> Option<String> {
    let name = self.current_function_name.as_ref()?;
    if self.beginner_functions.contains(name) {
        Some(name.clone())
    } else {
        None
    }
}
```

- [ ] **Step 7: Add `patch_program_types()` to `SemanticAnalyzer`**

```rust
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
```

- [ ] **Step 8: Handle `Type::Inferred` in `get_llvm_type` in codegen as a safety guard**

In `src/codegen.rs`, find `get_llvm_type()` and add:

```rust
Type::Inferred => panic!(
    "Type::Inferred reached codegen — patch_program_types() was not called"
),
```

- [ ] **Step 9: Run tests to verify they pass**

```bash
cargo test test_inferred_types 2>&1 | tail -10
```

Expected: both PASS.

- [ ] **Step 10: Commit**

```bash
git add src/semantic.rs src/codegen.rs
git commit -m "Resolve Type::Inferred params from call sites in semantic analysis"
```

---

## Task 6: Wire `patch_program_types` in main and detect beginner mode

**Files:**
- Modify: `src/main.rs`

- [ ] **Step 1: Write the test**

This is an integration step — test by compiling a beginner file end-to-end.

Create `examples/beginner_squares.eng`:
```
use beginner.

To square x:
    let result be a standard number with value x.
    Multiply result by x.
    Give back result.
End.

output square of 3.
output square of 5.
```

- [ ] **Step 2: Run it to verify it currently fails**

```bash
cargo run -- examples/beginner_squares.eng 2>&1
```

Expected: compile error (Type::Inferred not resolved before codegen).

- [ ] **Step 3: Add `patch_program_types` call in `main()`**

In `src/main.rs`, after semantic analysis succeeds, add the patch call:

```rust
// Semantic analysis
let mut analyzer = SemanticAnalyzer::new();
if let Err(errors) = analyzer.analyze(&program) {
    // error formatting handled below
    eprintln!("Semantic errors:");
    for error in errors {
        eprintln!("  {}", error);
    }
    std::process::exit(1);
}

// Resolve beginner-mode Inferred types in the AST before codegen
analyzer.patch_program_types(&mut program);
```

- [ ] **Step 4: Run the squares example to verify it compiles and runs**

```bash
cargo run -- examples/beginner_squares.eng && ./beginner_squares
```

Expected:
```
Compiled successfully: beginner_squares
9
25
```

- [ ] **Step 5: Commit**

```bash
git add src/main.rs examples/beginner_squares.eng
git commit -m "Call patch_program_types before codegen; add beginner_squares example"
```

---

## Task 7: Beginner-mode error messages

**Files:**
- Modify: `src/semantic.rs`, `src/main.rs`

- [ ] **Step 1: Add `to_beginner_string()` to `SemanticError`**

In `src/semantic.rs`, add this method to `impl SemanticError` (after the `thiserror` derive):

```rust
impl SemanticError {
    pub fn to_beginner_string(&self) -> String {
        let (line_part, description, try_hint) = match self {
            SemanticError::UndefinedVariable(name, line) => (
                format!("Line {line}"),
                format!("'{name}' hasn't been created yet."),
                format!("Add `let {name} be a standard number with value ...` before this line."),
            ),
            SemanticError::TypeMismatch { expected, found, line } => (
                format!("Line {line}"),
                format!("You're using a {found} where a {expected} is expected."),
                format!("Check that you're passing the right kind of value."),
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
            other => {
                let msg = format!("{other}");
                return format!("Oops! Something went wrong.\n  {msg}\n  Try: Check the line carefully.");
            }
        };
        format!("Oops! {line_part} has a problem.\n  {description}\n  Try: {try_hint}")
    }
}
```

- [ ] **Step 2: Add beginner parse error formatter in `main.rs`**

Add this helper function in `src/main.rs` (before `fn main()`):

```rust
fn format_beginner_parse_error(e: &str) -> String {
    // Extract "Line N:" prefix if present, reformat as Oops
    if let Some(rest) = e.strip_prefix("Line ") {
        if let Some(colon_pos) = rest.find(':') {
            let line_num = &rest[..colon_pos];
            let message = rest[colon_pos + 1..].trim();
            return format!(
                "Oops! Line {line_num} has a problem.\n  {message}\n  Try: Check the line carefully."
            );
        }
    }
    // Fallback for errors without a line number
    if e.contains("ended too early") || e.contains("missing an 'End.'") {
        return format!(
            "Oops! Something isn't finished.\n  {e}\n  Try: Add `End.` after the last line of the block."
        );
    }
    format!("Oops! Something went wrong.\n  {e}\n  Try: Check the line carefully.")
}
```

- [ ] **Step 3: Pre-detect beginner mode from source text in `main()`**

In `main()`, after reading the source file, add:

```rust
let is_beginner = source
    .lines()
    .find(|l| !l.trim().is_empty())
    .map(|l| l.trim() == "use beginner.")
    .unwrap_or(false);
```

- [ ] **Step 4: Route errors through beginner formatter**

Replace the existing error-printing blocks in `main()`:

```rust
// Parse errors:
let program = match load_program_with_imports(&source, source_dir, &mut HashSet::new()) {
    Ok(p) => p,
    Err(e) => {
        if is_beginner {
            println!("{}", format_beginner_parse_error(&e));
        } else {
            eprintln!("Error: {}", e);
        }
        std::process::exit(1);
    }
};

// Semantic errors:
if let Err(errors) = analyzer.analyze(&program) {
    if is_beginner {
        for error in &errors {
            println!("{}", error.to_beginner_string());
        }
    } else {
        eprintln!("Semantic errors:");
        for error in &errors {
            eprintln!("  {}", error);
        }
    }
    std::process::exit(1);
}
```

- [ ] **Step 5: Add a test example that triggers a beginner error**

Create `examples/beginner_greeting.eng`:
```
use beginner.

To greet name:
    output name.
End.

Call greet with "Alice".
```

Run it to verify it works:
```bash
cargo run -- examples/beginner_greeting.eng && ./beginner_greeting
```

Expected output:
```
Compiled successfully: beginner_greeting
Alice
```

Now test that an error produces the `Oops!` format. Create a temporary `examples/beginner_error_test.eng`:
```
use beginner.
output missing.
```

Run:
```bash
cargo run -- examples/beginner_error_test.eng 2>&1
```

Expected:
```
Oops! Line 2 has a problem.
  'missing' hasn't been created yet.
  Try: Add `let missing be a standard number with value ...` before this line.
```

Delete the temp file after verifying.

- [ ] **Step 6: Commit**

```bash
git add src/semantic.rs src/main.rs examples/beginner_greeting.eng
git commit -m "Add beginner-mode Oops! error messages"
```

---

## Task 8: Run full test suite and verify examples

- [ ] **Step 1: Run all unit tests**

```bash
cargo test 2>&1 | tail -30
```

Expected: all tests pass.

- [ ] **Step 2: Compile and run all original examples to verify no regressions**

```bash
for f in examples/hello examples/variables examples/loops examples/functions examples/fizzbuzz examples/fibonacci examples/arrays examples/strings examples/math; do
    echo "=== $f ===" && cargo run -- $f.eng 2>&1 | head -3
done
```

Expected: each example compiles and runs without error.

- [ ] **Step 3: Compile and run both new beginner examples**

```bash
cargo run -- examples/beginner_squares.eng && ./beginner_squares
cargo run -- examples/beginner_greeting.eng && ./beginner_greeting
```

Expected:
```
9
25
Alice
```

- [ ] **Step 4: Commit**

```bash
git commit -m "All tests pass; beginner mode fully wired"
```

---

## Self-Review Checklist

- **Spec coverage**: `use beginner.` header ✓, simplified func defs ✓, simplified func calls ✓, beginner errors ✓, unchanged syntax (control flow, vars, output) ✓
- **No placeholders**: all code blocks are complete
- **Type consistency**: `Type::Inferred` used in ast.rs, parser.rs (params and return), semantic.rs (inference fields and patch), codegen.rs (panic guard) — consistent throughout
- **Zero-arg beginner functions**: defined with `To name:` (already handled by existing `Colon` check in `is_function_def`), called with `Call name.` or `the result of name` — unchanged forms, no gap
- **`and` ambiguity in argument lists**: `parse_argument_list()` already parses at `parse_comparison()` level, so `and` inside `name of x and y` is consumed as a separator before `parse_and()` sees it — no ambiguity
