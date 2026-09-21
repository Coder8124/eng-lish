mod cluster;
mod list;
mod tensor;
mod watched;

use std::ffi::{CString, c_char, c_int};

pub use cluster::englang_kMeans;

unsafe extern "C" {
    fn printf(format: *const c_char, ...) -> c_int;
    fn malloc(size: usize) -> *mut c_char;
}

fn fail(message: &str) -> ! {
    eprintln!("Error: {}", message);
    std::process::exit(1);
}

fn format_number(number: f64) -> String {
    if number.is_nan() {
        return "not a number".to_string();
    }
    if number.is_infinite() {
        return if number > 0.0 { "infinity" } else { "-infinity" }.to_string();
    }
    let rounded: f64 = format!("{number:.9e}").parse().unwrap_or(number);
    let size = rounded.abs();
    if size != 0.0 && !(1e-9..1e16).contains(&size) {
        let shown = format!("{rounded:e}");
        let (digits, power) = shown.split_once('e').unwrap_or((&shown, "0"));
        let digits = if digits.contains('.') { digits.to_string() } else { format!("{digits}.0") };
        return format!("{digits}e{power}");
    }
    let shown = rounded.to_string();
    if shown.contains('.') { shown } else { format!("{shown}.0") }
}

#[unsafe(no_mangle)]
pub extern "C" fn englang_print_decimal(number: f64) {
    print_line(&format_number(number));
}

#[unsafe(no_mangle)]
pub extern "C" fn englang_decimal_text(number: f64) -> *mut c_char {
    let text = format_number(number);
    unsafe {
        let buffer = malloc(text.len() + 1);
        std::ptr::copy_nonoverlapping(text.as_ptr().cast(), buffer, text.len());
        *buffer.add(text.len()) = 0;
        buffer
    }
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

    #[test]
    fn tiny_and_huge_numbers_print_cleanly() {
        assert_eq!(format_number(0.000001), "0.000001");
        assert_eq!(format_number(1234567.5), "1234567.5");
        assert_eq!(format_number(1e20), "1.0e20");
        assert_eq!(format_number(-2.5e-12), "-2.5e-12");
        assert_eq!(format_number(0.0), "0.0");
        assert_eq!(format_number(f64::INFINITY), "infinity");
        assert_eq!(format_number(f64::NAN), "not a number");
    }
}
