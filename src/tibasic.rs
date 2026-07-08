//! TI-BASIC backend: translates a checked subset of eng-lish into TI-BASIC
//! source for TI-83/84 calculators.

use crate::ast::*;
use std::collections::HashMap;

pub fn compile(program: &Program) -> Result<String, Vec<String>> {
    let mut emitter = Emitter::new();
    emitter.emit_program(program);
    if emitter.errors.is_empty() {
        Ok(emitter.lines.join("\n") + "\n")
    } else {
        Err(emitter.errors)
    }
}

struct Emitter {
    lines: Vec<String>,
    errors: Vec<String>,
    numeric_vars: HashMap<String, char>,
    string_vars: HashMap<String, u8>,
    list_vars: HashMap<String, u8>,
    next_letter: u8,
    next_string: u8,
    next_list: u8,
    current_line: usize,
}

impl Emitter {
    fn new() -> Self {
        Self {
            lines: Vec::new(),
            errors: Vec::new(),
            numeric_vars: HashMap::new(),
            string_vars: HashMap::new(),
            list_vars: HashMap::new(),
            next_letter: 0,
            next_string: 0,
            next_list: 0,
            current_line: 0,
        }
    }

    fn error(&mut self, msg: &str) {
        self.errors
            .push(format!("Line {}: {}", self.current_line, msg));
    }

    fn numeric_var(&mut self, name: &str) -> String {
        if let Some(c) = self.numeric_vars.get(name) {
            return c.to_string();
        }
        if self.next_letter >= 26 {
            self.error("Too many number variables — calculators only have A through Z.");
            return "A".to_string();
        }
        let c = (b'A' + self.next_letter) as char;
        self.next_letter += 1;
        self.numeric_vars.insert(name.to_string(), c);
        c.to_string()
    }

    fn string_var(&mut self, name: &str) -> String {
        if let Some(n) = self.string_vars.get(name) {
            return format!("Str{}", n);
        }
        if self.next_string >= 10 {
            self.error("Too many text variables — calculators only have Str0 through Str9.");
            return "Str0".to_string();
        }
        let n = self.next_string;
        self.next_string += 1;
        self.string_vars.insert(name.to_string(), n);
        format!("Str{}", n)
    }

    fn list_var(&mut self, name: &str) -> String {
        if let Some(n) = self.list_vars.get(name) {
            return format!("L{}", n);
        }
        if self.next_list >= 6 {
            self.error("Too many lists — calculators only have L1 through L6.");
            return "L1".to_string();
        }
        let n = self.next_list + 1;
        self.next_list += 1;
        self.list_vars.insert(name.to_string(), n);
        format!("L{}", n)
    }

    fn var(&mut self, name: &str) -> String {
        if self.string_vars.contains_key(name) {
            self.string_var(name)
        } else if self.list_vars.contains_key(name) {
            self.list_var(name)
        } else {
            self.numeric_var(name)
        }
    }

    fn var_for_type(&mut self, name: &str, var_type: &Type) -> String {
        match var_type {
            Type::Text => self.string_var(name),
            Type::List(_) => self.list_var(name),
            Type::Int | Type::Float | Type::Bool | Type::Inferred => self.numeric_var(name),
            other => {
                self.error(&format!(
                    "The type '{}' doesn't exist on calculators.",
                    other
                ));
                self.numeric_var(name)
            }
        }
    }

    fn emit_program(&mut self, program: &Program) {
        if !program.classes.is_empty() {
            self.errors
                .push("Kinds (classes) aren't supported on calculators yet.".to_string());
        }
        if !program.functions.is_empty() {
            self.errors.push(
                "Functions and packages aren't supported on calculators yet — write your steps at the top level.".to_string(),
            );
        }
        for (i, stmt) in program.statements.iter().enumerate() {
            self.current_line = program
                .statement_lines
                .get(i)
                .copied()
                .unwrap_or(self.current_line);
            self.emit_statement(stmt);
        }
    }

    fn emit_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::VariableDecl {
                name,
                var_type,
                value,
            } => {
                let var = self.var_for_type(name, var_type);
                self.emit_store(value, &var);
            }
            Statement::Assignment { name, value } => {
                let var = self.var(name);
                self.emit_store(value, &var);
            }
            Statement::CompoundAssignment { name, op, value } => {
                let var = self.var(name);
                let val = self.emit_expr(value);
                let symbol = match op {
                    BinaryOp::Add => "+",
                    BinaryOp::Subtract => "-",
                    BinaryOp::Multiply => "*",
                    BinaryOp::Divide => "/",
                    _ => {
                        self.error("This operation isn't supported on calculators.");
                        "+"
                    }
                };
                self.lines.push(format!("{}{}{}→{}", var, symbol, val, var));
            }
            Statement::Output(expr) => {
                let val = self.emit_expr(expr);
                self.lines.push(format!("Disp {}", val));
            }
            Statement::If {
                condition,
                then_block,
                else_ifs,
                else_block,
            } => {
                self.emit_if(condition, then_block, else_ifs, else_block);
            }
            Statement::While { condition, body } => {
                let cond = self.emit_expr(condition);
                self.lines.push(format!("While {}", cond));
                for s in body {
                    self.emit_statement(s);
                }
                self.lines.push("End".to_string());
            }
            Statement::For {
                variable,
                start,
                end,
                body,
            } => {
                let var = self.numeric_var(variable);
                let start = self.emit_expr(start);
                let end = self.emit_expr(end);
                self.lines.push(format!("For({},{},{})", var, start, end));
                for s in body {
                    self.emit_statement(s);
                }
                self.lines.push("End".to_string());
            }
            Statement::IndexAssignment {
                collection,
                index,
                value,
            } => {
                let list = self.list_var(collection);
                let idx = self.emit_index(index);
                let val = self.emit_expr(value);
                self.lines.push(format!("{}→{}({})", val, list, idx));
            }
            Statement::ExprStatement(expr) => {
                if let Expr::FunctionCall { name, .. } = expr
                    && matches!(name.as_str(), "clearScreen" | "clear")
                {
                    self.lines.push("ClrHome".to_string());
                    return;
                }
                let val = self.emit_expr(expr);
                self.lines.push(val);
            }
            Statement::Break => {
                self.error("'stop.' isn't supported on calculators — try changing your loop condition instead.");
            }
            Statement::Continue => {
                self.error("'skip.' isn't supported on calculators — try an If around the rest of the loop instead.");
            }
            Statement::Return(_) => {
                self.error("'Give back' only works inside functions, which calculators don't support yet.");
            }
            Statement::PropertyAssignment { .. } => {
                self.error("Kinds (classes) aren't supported on calculators yet.");
            }
            Statement::Plot { .. } => {
                self.error("'plot' isn't supported on calculators yet.");
            }
        }
    }

    fn emit_store(&mut self, value: &Expr, var: &str) {
        if let Expr::FunctionCall { name, .. } = value {
            match name.as_str() {
                "readNumber" | "readnum" | "readLine" | "readln" | "input" => {
                    self.lines.push(format!("Input {}", var));
                    return;
                }
                _ => {}
            }
        }
        let val = self.emit_expr(value);
        self.lines.push(format!("{}→{}", val, var));
    }

    fn emit_if(
        &mut self,
        condition: &Expr,
        then_block: &[Statement],
        else_ifs: &[(Expr, Vec<Statement>)],
        else_block: &Option<Vec<Statement>>,
    ) {
        let cond = self.emit_expr(condition);
        self.lines.push(format!("If {}", cond));
        self.lines.push("Then".to_string());
        for s in then_block {
            self.emit_statement(s);
        }
        if !else_ifs.is_empty() {
            self.lines.push("Else".to_string());
            let (next_cond, next_block) = &else_ifs[0];
            self.emit_if(next_cond, next_block, &else_ifs[1..], else_block);
        } else if let Some(else_stmts) = else_block {
            self.lines.push("Else".to_string());
            for s in else_stmts {
                self.emit_statement(s);
            }
        }
        self.lines.push("End".to_string());
    }

    /// eng-lish indexes from 0, TI-BASIC lists from 1.
    fn emit_index(&mut self, index: &Expr) -> String {
        match index {
            Expr::IntLiteral(n) => format!("{}", n + 1),
            other => format!("{}+1", self.emit_expr(other)),
        }
    }

    fn emit_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::IntLiteral(n) => format!("{}", n),
            Expr::FloatLiteral(f) => format!("{}", f),
            Expr::StringLiteral(s) => format!("\"{}\"", s),
            Expr::BoolLiteral(b) => if *b { "1" } else { "0" }.to_string(),
            Expr::Identifier(name) => self.var(name),
            Expr::UnaryOp { op, operand } => {
                let val = self.emit_expr(operand);
                match op {
                    UnaryOp::Negate => format!("-{}", val),
                    UnaryOp::Not => format!("not({})", val),
                }
            }
            Expr::BinaryOp { op, left, right } => {
                let l = self.emit_expr(left);
                let r = self.emit_expr(right);
                match op {
                    BinaryOp::Add => format!("({}+{})", l, r),
                    BinaryOp::Subtract => format!("({}-{})", l, r),
                    BinaryOp::Multiply => format!("({}*{})", l, r),
                    BinaryOp::Divide => format!("({}/{})", l, r),
                    BinaryOp::Remainder => format!("remainder({},{})", l, r),
                    BinaryOp::Quotient => format!("int({}/{})", l, r),
                    BinaryOp::Equal => format!("{}={}", l, r),
                    BinaryOp::NotEqual => format!("{}≠{}", l, r),
                    BinaryOp::Greater => format!("{}>{}", l, r),
                    BinaryOp::Less => format!("{}<{}", l, r),
                    BinaryOp::GreaterEq => format!("{}≥{}", l, r),
                    BinaryOp::LessEq => format!("{}≤{}", l, r),
                    BinaryOp::And => format!("({} and {})", l, r),
                    BinaryOp::Or => format!("({} or {})", l, r),
                }
            }
            Expr::ListLiteral(elements) => {
                let parts: Vec<String> = elements.iter().map(|e| self.emit_expr(e)).collect();
                format!("{{{}}}", parts.join(","))
            }
            Expr::Index { collection, index } => {
                let coll = match collection.as_ref() {
                    Expr::Identifier(name) => self.list_var(name),
                    _ => {
                        self.error("Only named lists can be indexed on calculators.");
                        "L1".to_string()
                    }
                };
                let idx = self.emit_index(index);
                format!("{}({})", coll, idx)
            }
            Expr::FunctionCall { name, arguments } => self.emit_call(name, arguments),
            Expr::TypeConversion { target_type, expr } => {
                let val = self.emit_expr(expr);
                match target_type {
                    Type::Int => format!("int({})", val),
                    Type::Float => val,
                    _ => {
                        self.error("This type conversion isn't supported on calculators.");
                        val
                    }
                }
            }
            Expr::NewObject { .. } | Expr::MethodCall { .. } | Expr::PropertyAccess { .. } => {
                self.error("Kinds (classes) aren't supported on calculators yet.");
                "0".to_string()
            }
        }
    }

    fn emit_call(&mut self, name: &str, arguments: &[Expr]) -> String {
        let args: Vec<String> = arguments.iter().map(|a| self.emit_expr(a)).collect();
        let one = |args: &[String]| args.first().cloned().unwrap_or_else(|| "0".to_string());
        match name {
            "squareRoot" | "sqrt" => format!("√({})", one(&args)),
            "absoluteValue" | "abs" => format!("abs({})", one(&args)),
            "round" => format!("round({},0)", one(&args)),
            "floor" => format!("int({})", one(&args)),
            "sine" | "sin" => format!("sin({})", one(&args)),
            "cosine" | "cos" => format!("cos({})", one(&args)),
            "tangent" | "tan" => format!("tan({})", one(&args)),
            "naturalLog" | "ln" => format!("ln({})", one(&args)),
            "logarithm" | "log" => format!("log({})", one(&args)),
            "power" | "pow" => format!("({})^({})", one(&args), args.get(1).cloned().unwrap_or_else(|| "0".to_string())),
            "minimum" | "min" => format!("min({},{})", one(&args), args.get(1).cloned().unwrap_or_else(|| "0".to_string())),
            "maximum" | "max" => format!("max({},{})", one(&args), args.get(1).cloned().unwrap_or_else(|| "0".to_string())),
            "random" => "rand".to_string(),
            "randomBetween" | "randomInt" => format!(
                "randInt({},{})",
                one(&args),
                args.get(1).cloned().unwrap_or_else(|| "0".to_string())
            ),
            "arrayLength" | "len" => format!("dim({})", one(&args)),
            "sum" => format!("sum({})", one(&args)),
            "mean" | "average" => format!("mean({})", one(&args)),
            "arrayMin" | "minOf" => format!("min({})", one(&args)),
            "arrayMax" | "maxOf" => format!("max({})", one(&args)),
            "lengthOf" | "strlen" => format!("length({})", one(&args)),
            "combine" | "concat" => format!(
                "({}+{})",
                one(&args),
                args.get(1).cloned().unwrap_or_else(|| "\"\"".to_string())
            ),
            other => {
                self.error(&format!(
                    "The function '{}' isn't available on calculators.",
                    other
                ));
                "0".to_string()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn emit(src: &str) -> String {
        let program = Parser::parse(src).unwrap();
        compile(&program).unwrap()
    }

    #[test]
    fn test_variables_and_output() {
        let out = emit("Let score be a standard number with value 5.\noutput score.");
        assert_eq!(out, "5→A\nDisp A\n");
    }

    #[test]
    fn test_if_else() {
        let out = emit(
            "Let score be a standard number with value 5.\nIf score is at least 3 then\noutput \"BIG\".\notherwise\noutput \"SMALL\".\nEnd.",
        );
        assert!(out.contains("If A≥3"));
        assert!(out.contains("Then"));
        assert!(out.contains("Else"));
        assert!(out.ends_with("End\n"));
    }

    #[test]
    fn test_for_loop_and_lists() {
        let out = emit(
            "Let nums be a list of standard number with value [1, 2, 3].\nFor each i from 0 to 2,\noutput nums[i].\nEnd.",
        );
        assert!(out.contains("{1,2,3}→L1"));
        assert!(out.contains("For(A,0,2)"));
        assert!(out.contains("Disp L1(A+1)"));
    }

    #[test]
    fn test_input() {
        let out = emit(
            "Let guess be a standard number with value the result of readNumber.",
        );
        assert_eq!(out, "Input A\n");
    }

    #[test]
    fn test_unsupported_features_error() {
        let program = Parser::parse("plot [1, 2] as a line chart to \"out.html\".").unwrap();
        let errors = compile(&program).unwrap_err();
        assert!(errors[0].contains("plot"));
    }

    #[test]
    fn test_text_variables() {
        let out = emit("Let msg be a text with value \"HI\".\noutput msg.");
        assert_eq!(out, "\"HI\"→Str0\nDisp Str0\n");
    }
}
