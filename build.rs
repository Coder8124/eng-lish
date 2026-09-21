use std::env;
use std::path::PathBuf;
use std::process::Command;

// Built with rustc directly rather than a nested cargo, which would block on
// the target-directory lock this build already holds.
fn main() {
    println!("cargo:rerun-if-changed=runtime/src");

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let rustc = env::var("RUSTC").unwrap_or_else(|_| "rustc".to_string());
    let target = env::var("TARGET").unwrap();

    let output = Command::new(rustc)
        .args([
            "--crate-type=staticlib",
            "--crate-name=englang_runtime",
            "--edition=2024",
            "-Copt-level=3",
            "-Cpanic=abort",
            "--print=native-static-libs",
            "--target",
            &target,
            "--out-dir",
        ])
        .arg(&out_dir)
        .arg("runtime/src/lib.rs")
        .output()
        .expect("failed to run rustc on the runtime crate");

    let stderr = String::from_utf8_lossy(&output.stderr);
    if !output.status.success() {
        panic!("building the runtime failed:\n{}", stderr);
    }

    let native_libs = stderr
        .lines()
        .find_map(|line| line.split_once("native-static-libs:"))
        .map(|(_, libs)| libs.trim().to_string())
        .unwrap_or_default();
    println!("cargo:rustc-env=ENGLANG_RUNTIME_NATIVE_LIBS={}", native_libs);
}
