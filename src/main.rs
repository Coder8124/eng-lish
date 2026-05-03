mod ast;
mod codegen;
mod lexer;
mod parser;
mod semantic;
mod stdlib;

use ast::Program;
use codegen::CodeGen;
use inkwell::context::Context;
use parser::Parser;
use semantic::SemanticAnalyzer;
use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn packages_dir() -> PathBuf {
    let home = env::var("HOME").unwrap_or_else(|_| ".".to_string());
    PathBuf::from(home).join(".eng-lish").join("packages")
}

fn resolve_package_path(name: &str, source_dir: &Path) -> Option<PathBuf> {
    let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let candidates = [
        source_dir.join(format!("{}.eng", name)),
        cwd.join("packages").join(name).join(format!("{}.eng", name)),
        cwd.join("packages").join(name).join("main.eng"),
        packages_dir().join(name).join(format!("{}.eng", name)),
        packages_dir().join(name).join("main.eng"),
    ];
    candidates.into_iter().find(|p| p.exists())
}

fn load_program_with_imports(
    source: &str,
    source_dir: &Path,
    visited: &mut HashSet<String>,
) -> Result<Program, String> {
    let mut program = Parser::parse(source).map_err(|e| e.to_string())?;

    let imports = std::mem::take(&mut program.imports);
    for name in imports {
        if visited.contains(&name) {
            continue;
        }
        visited.insert(name.clone());

        let path = resolve_package_path(&name, source_dir).ok_or_else(|| {
            format!(
                "Package '{}' not found. Run: englishc install <github-url>",
                name
            )
        })?;

        let pkg_source = fs::read_to_string(&path)
            .map_err(|e| format!("Error reading package '{}': {}", name, e))?;
        let pkg_dir = path.parent().unwrap_or(source_dir);
        let pkg_program = load_program_with_imports(&pkg_source, pkg_dir, visited)?;
        program.merge(pkg_program);
    }

    Ok(program)
}

fn cmd_install(args: &[String]) {
    if args.is_empty() {
        eprintln!("Usage: englishc install <github-url>");
        std::process::exit(1);
    }
    let url = &args[0];
    let pkg_name = url
        .trim_end_matches('/')
        .split('/')
        .last()
        .unwrap_or("package")
        .trim_end_matches(".git")
        .to_string();

    let dest = packages_dir().join(&pkg_name);
    if dest.exists() {
        println!("Package '{}' is already installed at {:?}", pkg_name, dest);
        std::process::exit(0);
    }

    if let Some(parent) = dest.parent() {
        if let Err(e) = fs::create_dir_all(parent) {
            eprintln!("Could not create packages directory: {}", e);
            std::process::exit(1);
        }
    }

    println!("Installing '{}' from {}...", pkg_name, url);
    let status = Command::new("git")
        .args(["clone", url, dest.to_str().unwrap()])
        .status();

    match status {
        Ok(s) if s.success() => println!("Installed '{}' successfully.", pkg_name),
        Ok(s) => {
            eprintln!("git clone failed with exit code: {:?}", s.code());
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("Failed to run git: {}", e);
            std::process::exit(1);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: englishc <source.eng> [--ir]");
        eprintln!("       englishc install <github-url>");
        eprintln!("Options:");
        eprintln!("  --ir    Print LLVM IR instead of compiling");
        std::process::exit(1);
    }

    if args[1] == "install" {
        cmd_install(&args[2..]);
        return;
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

    let source_dir = Path::new(source_path)
        .parent()
        .unwrap_or_else(|| Path::new("."));

    // Parse the source, resolving imports
    let mut program = match load_program_with_imports(&source, source_dir, &mut HashSet::new()) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Error: {}", e);
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

    // Resolve beginner-mode Inferred types before codegen
    analyzer.patch_program_types(&mut program);

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
