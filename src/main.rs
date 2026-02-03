mod ast;
mod codegen;
mod lexer;
mod parser;
mod semantic;
mod stdlib;

use codegen::CodeGen;
use inkwell::context::Context;
use parser::Parser;
use semantic::SemanticAnalyzer;
use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: englishc <source.eng> [--ir]");
        eprintln!("Options:");
        eprintln!("  --ir    Print LLVM IR instead of compiling");
        std::process::exit(1);
    }

    let source_path = &args[1];
    let print_ir = args.contains(&"--ir".to_string());

    let source = match fs::read_to_string(source_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", source_path, e);
            std::process::exit(1);
        }
    };

    // Parse the source
    let program = match Parser::parse(&source) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            std::process::exit(1);
        }
    };

    // Semantic analysis
    let mut analyzer = SemanticAnalyzer::new();
    if let Err(errors) = analyzer.analyze(&program) {
        eprintln!("Semantic errors:");
        for error in errors {
            eprintln!("  {}", error);
        }
        std::process::exit(1);
    }

    // Code generation
    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "english_program");

    if let Err(e) = codegen.compile(&program) {
        eprintln!("Code generation error: {}", e);
        std::process::exit(1);
    }

    if print_ir {
        codegen.print_ir();
        return;
    }

    // Write object file
    let source_stem = Path::new(source_path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("output");

    let obj_path = format!("{}.o", source_stem);
    let exe_path = source_stem.to_string();

    if let Err(e) = codegen.write_object_file(Path::new(&obj_path)) {
        eprintln!("Error writing object file: {}", e);
        std::process::exit(1);
    }

    // Link with clang
    let status = Command::new("clang")
        .args([&obj_path, "-o", &exe_path, "-lm"])
        .status();

    match status {
        Ok(s) if s.success() => {
            println!("Compiled successfully: {}", exe_path);
            // Clean up object file
            let _ = fs::remove_file(&obj_path);
        }
        Ok(s) => {
            eprintln!("Linking failed with exit code: {:?}", s.code());
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("Failed to run linker: {}", e);
            eprintln!("Make sure clang is installed and in your PATH");
            std::process::exit(1);
        }
    }
}
