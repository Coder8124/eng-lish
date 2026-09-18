use crate::fail;
use std::cell::{Cell, RefCell};
use std::collections::HashSet;
use std::ffi::{CStr, CString, c_char, c_int};
use std::rc::Rc;

unsafe extern "C" {
    fn printf(format: *const c_char, ...) -> c_int;
}

#[derive(Clone, Copy)]
enum Op {
    Leaf,
    Constant,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Power(f64),
    Sigmoid,
    Relu,
    Tanh,
    Exponential,
    Logarithm,
}

pub struct Value {
    data: Cell<f64>,
    gradient: Cell<f64>,
    has_gradient: Cell<bool>,
    op: Op,
    children: Vec<Rc<Value>>,
    name: RefCell<Option<String>>,
}

// Dropping a long chain recursively would overflow the stack, so children are
// released one at a time from a work list instead.
impl Drop for Value {
    fn drop(&mut self) {
        let mut pending = std::mem::take(&mut self.children);
        while let Some(child) = pending.pop() {
            if let Ok(mut owned) = Rc::try_unwrap(child) {
                pending.append(&mut owned.children);
            }
        }
    }
}

type Handle = *const Value;

fn make(data: f64, op: Op, children: Vec<Rc<Value>>) -> Handle {
    Rc::into_raw(Rc::new(Value {
        data: Cell::new(data),
        gradient: Cell::new(0.0),
        has_gradient: Cell::new(false),
        op,
        children,
        name: RefCell::new(None),
    }))
}

unsafe fn get<'a>(handle: Handle) -> &'a Value {
    unsafe { &*handle }
}

unsafe fn share(handle: Handle) -> Rc<Value> {
    unsafe {
        Rc::increment_strong_count(handle);
        Rc::from_raw(handle)
    }
}

unsafe fn unary(a: Handle, op: Op, data: f64) -> Handle {
    unsafe { make(data, op, vec![share(a)]) }
}

unsafe fn binary(a: Handle, b: Handle, op: Op, data: f64) -> Handle {
    unsafe { make(data, op, vec![share(a), share(b)]) }
}

unsafe fn text(name: *const c_char) -> String {
    unsafe { CStr::from_ptr(name).to_string_lossy().into_owned() }
}

fn describe(value: &Value) -> String {
    match value.name.borrow().as_ref() {
        Some(name) => format!("'{name}'"),
        None => "this watched decimal".to_string(),
    }
}

fn format_number(number: f64) -> String {
    if !number.is_finite() {
        return number.to_string();
    }
    let rounded: f64 = format!("{number:.9e}").parse().unwrap_or(number);
    let shown = rounded.to_string();
    if shown.contains('.') { shown } else { format!("{shown}.0") }
}

fn print_line(line: &str) {
    let line = CString::new(line).unwrap_or_default();
    unsafe { printf(c"%s\n".as_ptr(), line.as_ptr()) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_new(data: f64, name: *const c_char) -> Handle {
    let handle = make(data, Op::Leaf, Vec::new());
    unsafe { *get(handle).name.borrow_mut() = Some(text(name)) };
    handle
}

#[unsafe(no_mangle)]
pub extern "C" fn englang_watched_constant(data: f64) -> Handle {
    make(data, Op::Constant, Vec::new())
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_name(handle: Handle, name: *const c_char) {
    let value = unsafe { get(handle) };
    let mut slot = value.name.borrow_mut();
    if slot.is_none() {
        *slot = Some(unsafe { text(name) });
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_retain(handle: Handle) {
    if !handle.is_null() {
        unsafe { Rc::increment_strong_count(handle) };
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_release(handle: Handle) {
    if !handle.is_null() {
        unsafe { drop(Rc::from_raw(handle)) };
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_value(handle: Handle) -> f64 {
    unsafe { get(handle).data.get() }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_gradient(handle: Handle) -> f64 {
    unsafe { get(handle).gradient.get() }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_add(a: Handle, b: Handle) -> Handle {
    unsafe { binary(a, b, Op::Add, get(a).data.get() + get(b).data.get()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_subtract(a: Handle, b: Handle) -> Handle {
    unsafe { binary(a, b, Op::Subtract, get(a).data.get() - get(b).data.get()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_multiply(a: Handle, b: Handle) -> Handle {
    unsafe { binary(a, b, Op::Multiply, get(a).data.get() * get(b).data.get()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_divide(a: Handle, b: Handle) -> Handle {
    unsafe { binary(a, b, Op::Divide, get(a).data.get() / get(b).data.get()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_negate(a: Handle) -> Handle {
    unsafe { unary(a, Op::Negate, -get(a).data.get()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_power(a: Handle, exponent: f64) -> Handle {
    unsafe { unary(a, Op::Power(exponent), get(a).data.get().powf(exponent)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_sigmoid(a: Handle) -> Handle {
    let x = unsafe { get(a).data.get() };
    unsafe { unary(a, Op::Sigmoid, 1.0 / (1.0 + (-x).exp())) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_relu(a: Handle) -> Handle {
    unsafe { unary(a, Op::Relu, get(a).data.get().max(0.0)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_tanh(a: Handle) -> Handle {
    unsafe { unary(a, Op::Tanh, get(a).data.get().tanh()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_exponential(a: Handle) -> Handle {
    unsafe { unary(a, Op::Exponential, get(a).data.get().exp()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_logarithm(a: Handle) -> Handle {
    let value = unsafe { get(a) };
    let x = value.data.get();
    if x <= 0.0 {
        fail(&format!(
            "You can only take the logarithm of a number bigger than 0, but {} is {}.",
            describe(value),
            format_number(x)
        ));
    }
    unsafe { unary(a, Op::Logarithm, x.ln()) }
}

// A plain number is folded straight into a leaf so the same watched decimal
// keeps being the one the graph points at; anything else gets a new node.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_update(target: Handle, op: c_int, amount: f64) -> Handle {
    let value = unsafe { get(target) };
    if matches!(value.op, Op::Leaf) {
        let old = value.data.get();
        value.data.set(match op {
            0 => old + amount,
            1 => old - amount,
            2 => old * amount,
            _ => old / amount,
        });
        unsafe { englang_watched_retain(target) };
        return target;
    }
    let constant = englang_watched_constant(amount);
    let result = unsafe {
        match op {
            0 => englang_watched_add(target, constant),
            1 => englang_watched_subtract(target, constant),
            2 => englang_watched_multiply(target, constant),
            _ => englang_watched_divide(target, constant),
        }
    };
    unsafe { englang_watched_release(constant) };
    result
}

fn in_order(root: &Rc<Value>) -> Vec<Rc<Value>> {
    let mut order = Vec::new();
    let mut seen = HashSet::new();
    let mut stack = vec![(Rc::clone(root), false)];
    while let Some((node, children_done)) = stack.pop() {
        if children_done {
            order.push(node);
            continue;
        }
        if !seen.insert(Rc::as_ptr(&node)) {
            continue;
        }
        stack.push((Rc::clone(&node), true));
        for child in &node.children {
            if !seen.contains(&Rc::as_ptr(child)) {
                stack.push((Rc::clone(child), false));
            }
        }
    }
    order
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_backward(root: Handle) {
    let root = unsafe { share(root) };
    let order = in_order(&root);
    for node in &order {
        node.gradient.set(0.0);
        node.has_gradient.set(true);
    }
    root.gradient.set(1.0);
    for node in order.iter().rev() {
        let upstream = node.gradient.get();
        let out = node.data.get();
        let push = |index: usize, amount: f64| {
            let child = &node.children[index];
            child.gradient.set(child.gradient.get() + amount);
        };
        let input = |index: usize| node.children[index].data.get();
        match node.op {
            Op::Leaf | Op::Constant => {}
            Op::Add => {
                push(0, upstream);
                push(1, upstream);
            }
            Op::Subtract => {
                push(0, upstream);
                push(1, -upstream);
            }
            Op::Multiply => {
                push(0, upstream * input(1));
                push(1, upstream * input(0));
            }
            Op::Divide => {
                push(0, upstream / input(1));
                push(1, -upstream * input(0) / (input(1) * input(1)));
            }
            Op::Negate => push(0, -upstream),
            Op::Power(exponent) => push(0, upstream * exponent * input(0).powf(exponent - 1.0)),
            Op::Sigmoid => push(0, upstream * out * (1.0 - out)),
            Op::Relu => push(0, if input(0) > 0.0 { upstream } else { 0.0 }),
            Op::Tanh => push(0, upstream * (1.0 - out * out)),
            Op::Exponential => push(0, upstream * out),
            Op::Logarithm => push(0, upstream / input(0)),
        }
    }
}

fn operation_word(op: Op) -> String {
    match op {
        Op::Leaf | Op::Constant => String::new(),
        Op::Add => "plus".to_string(),
        Op::Subtract => "minus".to_string(),
        Op::Multiply => "times".to_string(),
        Op::Divide => "divided by".to_string(),
        Op::Negate => "negative".to_string(),
        Op::Power(exponent) => format!("to the power {}", format_number(exponent)),
        Op::Sigmoid => "sigmoid".to_string(),
        Op::Relu => "relu".to_string(),
        Op::Tanh => "tanh".to_string(),
        Op::Exponential => "exponential".to_string(),
        Op::Logarithm => "logarithm".to_string(),
    }
}

fn graph_line(node: &Value) -> String {
    let data = format_number(node.data.get());
    let mut line = match (&node.op, node.name.borrow().as_ref()) {
        (Op::Constant, _) => data,
        (Op::Leaf, Some(name)) => format!("{name} = {data}"),
        (Op::Leaf, None) => data,
        (op, Some(name)) => format!("{name} = {data}  ({})", operation_word(*op)),
        (op, None) => format!("{} = {data}", operation_word(*op)),
    };
    if node.has_gradient.get() && !matches!(node.op, Op::Constant) {
        line.push_str(&format!("   gradient {}", format_number(node.gradient.get())));
    }
    line
}

fn show(node: &Rc<Value>, prefix: &str, last: bool, top: bool, shown: &mut HashSet<*const Value>) {
    let branch = if top { "" } else if last { "└── " } else { "├── " };
    let repeat = !node.children.is_empty() && !shown.insert(Rc::as_ptr(node));
    let suffix = if repeat { "   (shown above)" } else { "" };
    print_line(&format!("{prefix}{branch}{}{suffix}", graph_line(node)));
    if repeat {
        return;
    }
    let inner = if top { String::new() } else if last { format!("{prefix}    ") } else { format!("{prefix}│   ") };
    for (index, child) in node.children.iter().enumerate() {
        show(child, &inner, index + 1 == node.children.len(), false, shown);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_watched_show(root: Handle) {
    let root = unsafe { share(root) };
    show(&root, "", true, true, &mut HashSet::new());
}

#[cfg(test)]
mod tests {
    use super::*;

    fn leaf(data: f64, name: &str) -> Handle {
        let name = CString::new(name).unwrap();
        unsafe { englang_watched_new(data, name.as_ptr()) }
    }

    #[test]
    fn chain_rule_through_a_square() {
        unsafe {
            let w = leaf(0.5, "w");
            let three = englang_watched_constant(3.0);
            let six = englang_watched_constant(6.0);
            let scaled = englang_watched_multiply(w, three);
            let error = englang_watched_subtract(scaled, six);
            let loss = englang_watched_multiply(error, error);
            englang_watched_backward(loss);
            assert_eq!(englang_watched_value(loss), 20.25);
            assert_eq!(englang_watched_gradient(w), -27.0);
            for handle in [loss, error, scaled, six, three, w] {
                englang_watched_release(handle);
            }
        }
    }

    #[test]
    fn gradients_match_finite_differences() {
        let build = |x: f64| unsafe {
            let w = leaf(x, "w");
            let two = englang_watched_constant(2.0);
            let s = englang_watched_sigmoid(w);
            let t = englang_watched_tanh(englang_watched_multiply(w, two));
            let e = englang_watched_exponential(englang_watched_negate(w));
            let l = englang_watched_logarithm(englang_watched_add(englang_watched_power(w, 2.0), two));
            let r = englang_watched_relu(englang_watched_divide(w, two));
            let total = englang_watched_add(
                englang_watched_add(englang_watched_add(s, t), englang_watched_add(e, l)),
                r,
            );
            (w, total)
        };
        let x = 0.7;
        let (w, total) = build(x);
        unsafe { englang_watched_backward(total) };
        let analytic = unsafe { englang_watched_gradient(w) };
        let step = 1e-6;
        let above = unsafe { englang_watched_value(build(x + step).1) };
        let below = unsafe { englang_watched_value(build(x - step).1) };
        let numeric = (above - below) / (2.0 * step);
        assert!((analytic - numeric).abs() < 1e-6, "{analytic} vs {numeric}");
    }

    #[test]
    fn gradients_reset_between_backward_passes() {
        unsafe {
            let w = leaf(2.0, "w");
            let square = englang_watched_multiply(w, w);
            englang_watched_backward(square);
            englang_watched_backward(square);
            assert_eq!(englang_watched_gradient(w), 4.0);
        }
    }

    #[test]
    fn updating_a_leaf_keeps_the_same_node() {
        unsafe {
            let w = leaf(1.0, "w");
            let same = englang_watched_update(w, 1, 0.25);
            assert_eq!(same, w);
            assert_eq!(englang_watched_value(w), 0.75);
            let doubled = englang_watched_multiply(w, w);
            let grown = englang_watched_update(doubled, 0, 1.0);
            assert_ne!(grown, doubled);
            assert_eq!(englang_watched_value(grown), 1.5625);
        }
    }

    #[test]
    fn releasing_a_long_chain_does_not_overflow() {
        unsafe {
            let mut total = englang_watched_constant(0.0);
            let one = englang_watched_constant(1.0);
            for _ in 0..1_000_000 {
                let next = englang_watched_add(total, one);
                englang_watched_release(total);
                total = next;
            }
            assert_eq!(englang_watched_value(total), 1_000_000.0);
            englang_watched_release(total);
            englang_watched_release(one);
        }
    }

    #[test]
    fn numbers_print_like_decimals() {
        assert_eq!(format_number(4.0), "4.0");
        assert_eq!(format_number(0.1 + 0.2), "0.3");
        assert_eq!(format_number(-27.0), "-27.0");
    }
}
