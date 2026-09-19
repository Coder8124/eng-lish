mod cluster;
mod list;
mod tensor;
mod watched;

use std::ffi::{CString, c_char, c_int};

pub use cluster::englang_kMeans;

unsafe extern "C" {
    fn printf(format: *const c_char, ...) -> c_int;
}

fn fail(message: &str) -> ! {
    eprintln!("Error: {}", message);
    std::process::exit(1);
}

fn format_number(number: f64) -> String {
    if !number.is_finite() {
        return number.to_string();
    }
    let rounded: f64 = format!("{number:.9e}").parse().unwrap_or(number);
    let shown = rounded.to_string();
    if shown.contains('.') { shown } else { format!("{shown}.0") }
}

// Printing goes through C's printf so it shares a buffer with the program's
// own output and lines come out in order.
fn print_line(line: &str) {
    let line = CString::new(line).unwrap_or_default();
    unsafe { printf(c"%s\n".as_ptr(), line.as_ptr()) };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numbers_print_like_decimals() {
        assert_eq!(format_number(4.0), "4.0");
        assert_eq!(format_number(0.1 + 0.2), "0.3");
        assert_eq!(format_number(-27.0), "-27.0");
    }
}
