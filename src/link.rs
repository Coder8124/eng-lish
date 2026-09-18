use std::fs;
use std::io;
use std::path::Path;
use std::process::{Command, ExitStatus};

const RUNTIME: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/libenglang_runtime.a"));
const NATIVE_LIBS: &str = env!("ENGLANG_RUNTIME_NATIVE_LIBS");

pub fn link(obj: &Path, exe: &Path) -> io::Result<ExitStatus> {
    let runtime = obj.with_extension("runtime.a");
    fs::write(&runtime, RUNTIME)?;

    let dead_strip = if cfg!(target_os = "macos") {
        "-Wl,-dead_strip"
    } else {
        "-Wl,--gc-sections"
    };
    let status = Command::new("clang")
        .arg(obj)
        .arg(&runtime)
        .arg("-o")
        .arg(exe)
        .args(["-lm", dead_strip])
        // clang already links these; repeating them makes macOS's ld warn.
        .args(
            NATIVE_LIBS
                .split_whitespace()
                .filter(|lib| !matches!(*lib, "-lm" | "-lc" | "-lSystem")),
        )
        .status();

    let _ = fs::remove_file(&runtime);
    status
}
