use crate::list::{as_slice, from_slice};
use crate::{fail, format_number, print_line};
use std::cell::Cell;
use std::rc::Rc;

pub struct Tensor {
    shape: Vec<usize>,
    data: Vec<f64>,
}

type Handle = *const Tensor;

fn make(shape: Vec<usize>, data: Vec<f64>) -> Handle {
    Rc::into_raw(Rc::new(Tensor { shape, data }))
}

unsafe fn get<'a>(handle: Handle) -> &'a Tensor {
    unsafe { &*handle }
}

fn describe(shape: &[usize]) -> String {
    match shape {
        [] => "single-number tensor".to_string(),
        [n] => format!("{n}-number tensor"),
        _ => {
            let sizes: Vec<String> = shape.iter().map(|n| n.to_string()).collect();
            format!("{} tensor", sizes.join("-by-"))
        }
    }
}

fn with_article(shape: &[usize]) -> String {
    let text = describe(shape);
    let vowel_sound = text.starts_with('8') || text.starts_with("11") || text.starts_with("18");
    format!("{} {text}", if vowel_sound { "an" } else { "a" })
}

fn dims_from(dims: *const i64, count: i64, action: &str) -> Vec<usize> {
    let dims = unsafe { std::slice::from_raw_parts(dims, count as usize) };
    dims.iter()
        .map(|&n| {
            if n < 1 {
                fail(&format!("{action} needs sizes of at least 1, but was given {n}."));
            }
            n as usize
        })
        .collect()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_retain(handle: Handle) {
    if !handle.is_null() {
        unsafe { Rc::increment_strong_count(handle) };
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_release(handle: Handle) {
    if !handle.is_null() {
        unsafe { drop(Rc::from_raw(handle)) };
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn englang_tensor_scalar(value: f64) -> Handle {
    make(Vec::new(), vec![value])
}

// Lists nest as a list of pointers to lists, each element one 8-byte slot, so
// a depth-2 list is read as pointers first and numbers at the bottom.
unsafe fn read_list(data: *const u64, depth: i64, is_int: bool, shape: &mut Vec<usize>, level: usize, out: &mut Vec<f64>) {
    let items = unsafe { as_slice(data) };
    if shape.len() == level {
        shape.push(items.len());
    } else if shape[level] != items.len() {
        fail(&format!(
            "Every row of a tensor needs the same length, but one row has {} numbers and another has {}.",
            shape[level],
            items.len()
        ));
    }
    for &item in items {
        if depth > 1 {
            unsafe { read_list(item as *const u64, depth - 1, is_int, shape, level + 1, out) };
        } else if is_int {
            out.push(item as i64 as f64);
        } else {
            out.push(f64::from_bits(item));
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_from_list(list: *const u64, depth: i64, is_int: i64) -> Handle {
    let mut shape = Vec::new();
    let mut data = Vec::new();
    unsafe { read_list(list, depth, is_int != 0, &mut shape, 0, &mut data) };
    if data.is_empty() {
        fail("A tensor needs at least one number, but this list is empty.");
    }
    make(shape, data)
}

thread_local! {
    static RANDOM_STATE: Cell<u64> = const { Cell::new(0x2545_F491_4F6C_DD1D) };
}

fn next_uniform() -> f64 {
    RANDOM_STATE.with(|state| {
        let mut x = state.get();
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        state.set(x);
        ((x.wrapping_mul(0x2545_F491_4F6C_DD1D) >> 11) as f64 + 0.5) / (1u64 << 53) as f64
    })
}

// Box-Muller turns two even-spread numbers into one bell-curve number. The
// seed is fixed, so every run (and every student) gets the same tensors.
fn next_normal() -> f64 {
    let (u, v) = (next_uniform(), next_uniform());
    (-2.0 * u.ln()).sqrt() * (std::f64::consts::TAU * v).cos()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_filled(kind: i64, dims: *const i64, count: i64) -> Handle {
    let names = ["zeroTensor", "oneTensor", "randomTensor"];
    let shape = dims_from(dims, count, names[kind as usize]);
    let size = shape.iter().product();
    let data = match kind {
        0 => vec![0.0; size],
        1 => vec![1.0; size],
        _ => (0..size).map(|_| next_normal()).collect(),
    };
    make(shape, data)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_reshape(handle: Handle, dims: *const i64, count: i64) -> Handle {
    let tensor = unsafe { get(handle) };
    let shape = dims_from(dims, count, "reshape");
    let size: usize = shape.iter().product();
    if size != tensor.data.len() {
        fail(&format!(
            "You can't reshape {} into {}: one holds {} numbers and the other holds {}.",
            with_article(&tensor.shape),
            with_article(&shape),
            tensor.data.len(),
            size
        ));
    }
    make(shape, tensor.data.clone())
}

fn strides(shape: &[usize]) -> Vec<usize> {
    let mut strides = vec![1; shape.len()];
    for i in (0..shape.len().saturating_sub(1)).rev() {
        strides[i] = strides[i + 1] * shape[i + 1];
    }
    strides
}

fn broadcast_shape(a: &[usize], b: &[usize], verb: &str) -> Vec<usize> {
    let rank = a.len().max(b.len());
    let size_at = |shape: &[usize], i: usize| {
        let offset = rank - shape.len();
        if i < offset { 1 } else { shape[i - offset] }
    };
    (0..rank)
        .map(|i| {
            let (x, y) = (size_at(a, i), size_at(b, i));
            if x == y || y == 1 {
                x
            } else if x == 1 {
                y
            } else {
                fail(&format!(
                    "You tried to {verb} {} and {}. Line their sizes up from the right: {x} and {y} don't match. Each pair needs to be the same, or one of them needs to be 1.",
                    with_article(a),
                    with_article(b)
                ))
            }
        })
        .collect()
}

// Where a tensor has size 1 along a dimension it is reused across that whole
// dimension, which is done by giving that dimension a stride of 0.
fn broadcast_strides(shape: &[usize], rank: usize) -> Vec<usize> {
    let own = strides(shape);
    let offset = rank - shape.len();
    (0..rank)
        .map(|i| if i < offset || shape[i - offset] == 1 { 0 } else { own[i - offset] })
        .collect()
}

fn zip_with(a: &Tensor, b: &Tensor, verb: &str, f: impl Fn(f64, f64) -> f64) -> Handle {
    let shape = broadcast_shape(&a.shape, &b.shape, verb);
    let (a_strides, b_strides) = (broadcast_strides(&a.shape, shape.len()), broadcast_strides(&b.shape, shape.len()));
    let size: usize = shape.iter().product();
    let mut index = vec![0; shape.len()];
    let mut data = Vec::with_capacity(size);
    for _ in 0..size {
        let a_at: usize = index.iter().zip(&a_strides).map(|(i, s)| i * s).sum();
        let b_at: usize = index.iter().zip(&b_strides).map(|(i, s)| i * s).sum();
        data.push(f(a.data[a_at], b.data[b_at]));
        for d in (0..shape.len()).rev() {
            index[d] += 1;
            if index[d] < shape[d] {
                break;
            }
            index[d] = 0;
        }
    }
    make(shape, data)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_add(a: Handle, b: Handle) -> Handle {
    unsafe { zip_with(get(a), get(b), "add", |x, y| x + y) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_subtract(a: Handle, b: Handle) -> Handle {
    unsafe { zip_with(get(a), get(b), "subtract", |x, y| x - y) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_multiply(a: Handle, b: Handle) -> Handle {
    unsafe { zip_with(get(a), get(b), "multiply", |x, y| x * y) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_divide(a: Handle, b: Handle) -> Handle {
    unsafe { zip_with(get(a), get(b), "divide", |x, y| x / y) }
}

fn map(handle: Handle, f: impl Fn(f64) -> f64) -> Handle {
    let tensor = unsafe { get(handle) };
    make(tensor.shape.clone(), tensor.data.iter().map(|&x| f(x)).collect())
}

#[unsafe(no_mangle)]
pub extern "C" fn englang_tensor_negate(a: Handle) -> Handle {
    map(a, |x| -x)
}

#[unsafe(no_mangle)]
pub extern "C" fn englang_tensor_power(a: Handle, exponent: f64) -> Handle {
    map(a, |x| x.powf(exponent))
}

#[unsafe(no_mangle)]
pub extern "C" fn englang_tensor_sigmoid(a: Handle) -> Handle {
    map(a, |x| 1.0 / (1.0 + (-x).exp()))
}

#[unsafe(no_mangle)]
pub extern "C" fn englang_tensor_relu(a: Handle) -> Handle {
    map(a, |x| x.max(0.0))
}

#[unsafe(no_mangle)]
pub extern "C" fn englang_tensor_tanh(a: Handle) -> Handle {
    map(a, f64::tanh)
}

#[unsafe(no_mangle)]
pub extern "C" fn englang_tensor_exponential(a: Handle) -> Handle {
    map(a, f64::exp)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_logarithm(a: Handle) -> Handle {
    if let Some(bad) = unsafe { get(a) }.data.iter().find(|&&x| x <= 0.0) {
        fail(&format!(
            "You can only take the logarithm of numbers bigger than 0, but this tensor holds {}.",
            format_number(*bad)
        ));
    }
    map(a, f64::ln)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_softmax(a: Handle) -> Handle {
    let tensor = unsafe { get(a) };
    let width = tensor.shape.last().copied().unwrap_or(1);
    let mut data = Vec::with_capacity(tensor.data.len());
    for row in tensor.data.chunks(width) {
        let biggest = row.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
        let powers: Vec<f64> = row.iter().map(|x| (x - biggest).exp()).collect();
        let total: f64 = powers.iter().sum();
        data.extend(powers.iter().map(|p| p / total));
    }
    make(tensor.shape.clone(), data)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_matmul(a: Handle, b: Handle) -> Handle {
    let (a, b) = unsafe { (get(a), get(b)) };
    if a.shape.is_empty() || b.shape.is_empty() || a.shape.len() > 2 || b.shape.len() > 2 {
        fail(&format!(
            "matmul works on tensors with 1 or 2 dimensions, but was given {} and {}.",
            with_article(&a.shape),
            with_article(&b.shape)
        ));
    }
    let (rows, inner) = if a.shape.len() == 2 { (a.shape[0], a.shape[1]) } else { (1, a.shape[0]) };
    let (inner_b, cols) = if b.shape.len() == 2 { (b.shape[0], b.shape[1]) } else { (b.shape[0], 1) };
    if inner != inner_b {
        fail(&format!(
            "You tried to matmul {} by {}. The {inner} and the {inner_b} need to match: each row of the first is multiplied by each column of the second, so they need the same length.",
            with_article(&a.shape),
            with_article(&b.shape)
        ));
    }
    let mut data = vec![0.0; rows * cols];
    for r in 0..rows {
        for k in 0..inner {
            let left = a.data[r * inner + k];
            for c in 0..cols {
                data[r * cols + c] += left * b.data[k * cols + c];
            }
        }
    }
    let mut shape = Vec::new();
    if a.shape.len() == 2 {
        shape.push(rows);
    }
    if b.shape.len() == 2 {
        shape.push(cols);
    }
    make(shape, data)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_transpose(handle: Handle) -> Handle {
    let tensor = unsafe { get(handle) };
    let shape: Vec<usize> = tensor.shape.iter().rev().cloned().collect();
    let old_strides = strides(&tensor.shape);
    let size = tensor.data.len();
    let mut index = vec![0; shape.len()];
    let mut data = Vec::with_capacity(size);
    for _ in 0..size {
        let at: usize = index.iter().zip(old_strides.iter().rev()).map(|(i, s)| i * s).sum();
        data.push(tensor.data[at]);
        for d in (0..shape.len()).rev() {
            index[d] += 1;
            if index[d] < shape[d] {
                break;
            }
            index[d] = 0;
        }
    }
    make(shape, data)
}

fn along(handle: Handle, axis: i64, action: &str, average: bool) -> Handle {
    let tensor = unsafe { get(handle) };
    let rank = tensor.shape.len();
    if axis < 0 || axis as usize >= rank {
        let choices = match rank {
            0 => "A single-number tensor has no directions to add along.".to_string(),
            1 => "It only has direction 0.".to_string(),
            _ => format!("Pick a direction from 0 to {}.", rank - 1),
        };
        fail(&format!("{action} can't go along direction {axis} of {}. {choices}", with_article(&tensor.shape)));
    }
    let axis = axis as usize;
    let outer: usize = tensor.shape[..axis].iter().product();
    let length = tensor.shape[axis];
    let inner: usize = tensor.shape[axis + 1..].iter().product();
    let mut data = vec![0.0; outer * inner];
    for o in 0..outer {
        for k in 0..length {
            for i in 0..inner {
                data[o * inner + i] += tensor.data[(o * length + k) * inner + i];
            }
        }
    }
    if average {
        data.iter_mut().for_each(|x| *x /= length as f64);
    }
    let mut shape = tensor.shape.clone();
    shape.remove(axis);
    make(shape, data)
}

#[unsafe(no_mangle)]
pub extern "C" fn englang_tensor_sum_along(a: Handle, axis: i64) -> Handle {
    along(a, axis, "sumAlong", false)
}

#[unsafe(no_mangle)]
pub extern "C" fn englang_tensor_mean_along(a: Handle, axis: i64) -> Handle {
    along(a, axis, "meanAlong", true)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_sum(a: Handle) -> Handle {
    englang_tensor_scalar(unsafe { get(a) }.data.iter().sum())
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_mean(a: Handle) -> Handle {
    let tensor = unsafe { get(a) };
    englang_tensor_scalar(tensor.data.iter().sum::<f64>() / tensor.data.len() as f64)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_shape(a: Handle) -> *mut i64 {
    let shape: Vec<i64> = unsafe { get(a) }.shape.iter().map(|&n| n as i64).collect();
    from_slice(&shape)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_value(a: Handle) -> f64 {
    let tensor = unsafe { get(a) };
    if tensor.data.len() != 1 {
        let count = if tensor.shape.len() > 1 {
            format!(" holding {} numbers", tensor.data.len())
        } else {
            String::new()
        };
        fail(&format!(
            "Only a tensor holding one number can become a decimal, but this is {}{count}.",
            with_article(&tensor.shape)
        ));
    }
    tensor.data[0]
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_index(a: Handle, index: i64) -> Handle {
    let tensor = unsafe { get(a) };
    let Some(&length) = tensor.shape.first() else {
        fail("A single-number tensor has no rows to pick from.");
    };
    if index < 0 || index as usize >= length {
        fail(&format!(
            "You asked for item {index} of {}, but it only has items 0 to {}.",
            with_article(&tensor.shape),
            length - 1
        ));
    }
    let inner = tensor.data.len() / length;
    let start = index as usize * inner;
    make(tensor.shape[1..].to_vec(), tensor.data[start..start + inner].to_vec())
}

const MAX_ROWS: usize = 20;

fn print_rows(shape: &[usize], data: &[f64], widths: &[usize]) {
    let columns = shape.last().copied().unwrap_or(1);
    let rows: Vec<&[f64]> = data.chunks(columns).collect();
    for row in rows.iter().take(MAX_ROWS) {
        let cells: Vec<String> = row
            .iter()
            .zip(widths)
            .map(|(&x, &width)| format!("{:>width$}", format_number(x)))
            .collect();
        print_line(&format!("[ {} ]", cells.join("  ")));
    }
    if rows.len() > MAX_ROWS {
        print_line(&format!("... and {} more rows", rows.len() - MAX_ROWS));
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn englang_tensor_print(a: Handle) {
    let tensor = unsafe { get(a) };
    if tensor.shape.is_empty() {
        print_line(&format_number(tensor.data[0]));
        return;
    }
    let columns = tensor.shape[tensor.shape.len() - 1];
    let mut widths = vec![1; columns];
    for (i, &x) in tensor.data.iter().enumerate() {
        widths[i % columns] = widths[i % columns].max(format_number(x).len());
    }
    let block: usize = tensor.shape[tensor.shape.len().saturating_sub(2)..].iter().product();
    for (i, chunk) in tensor.data.chunks(block).enumerate() {
        if i > 0 {
            print_line("");
        }
        print_rows(&tensor.shape[tensor.shape.len().saturating_sub(2)..], chunk, &widths);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tensor(shape: &[usize], data: &[f64]) -> Handle {
        make(shape.to_vec(), data.to_vec())
    }

    fn contents(handle: Handle) -> (Vec<usize>, Vec<f64>) {
        let t = unsafe { get(handle) };
        (t.shape.clone(), t.data.clone())
    }

    #[test]
    fn adding_a_row_to_every_row_broadcasts() {
        let grid = tensor(&[2, 3], &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
        let row = tensor(&[3], &[10.0, 20.0, 30.0]);
        let sum = unsafe { englang_tensor_add(grid, row) };
        assert_eq!(contents(sum), (vec![2, 3], vec![11.0, 22.0, 33.0, 14.0, 25.0, 36.0]));
        let column = tensor(&[2, 1], &[100.0, 200.0]);
        let sum = unsafe { englang_tensor_add(grid, column) };
        assert_eq!(contents(sum).1, vec![101.0, 102.0, 103.0, 204.0, 205.0, 206.0]);
        let scaled = unsafe { englang_tensor_multiply(grid, englang_tensor_scalar(2.0)) };
        assert_eq!(contents(scaled).1, vec![2.0, 4.0, 6.0, 8.0, 10.0, 12.0]);
    }

    #[test]
    fn matmul_multiplies_rows_by_columns() {
        let a = tensor(&[2, 3], &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
        let b = tensor(&[3, 2], &[7.0, 8.0, 9.0, 10.0, 11.0, 12.0]);
        assert_eq!(contents(unsafe { englang_tensor_matmul(a, b) }), (vec![2, 2], vec![58.0, 64.0, 139.0, 154.0]));
        let v = tensor(&[3], &[1.0, 0.0, 1.0]);
        assert_eq!(contents(unsafe { englang_tensor_matmul(a, v) }), (vec![2], vec![4.0, 10.0]));
        assert_eq!(contents(unsafe { englang_tensor_matmul(v, v) }), (vec![], vec![2.0]));
    }

    #[test]
    fn transpose_swaps_rows_and_columns() {
        let a = tensor(&[2, 3], &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
        assert_eq!(contents(unsafe { englang_tensor_transpose(a) }), (vec![3, 2], vec![1.0, 4.0, 2.0, 5.0, 3.0, 6.0]));
    }

    #[test]
    fn sums_and_means_along_a_direction() {
        let a = tensor(&[2, 3], &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
        assert_eq!(contents(englang_tensor_sum_along(a, 0)), (vec![3], vec![5.0, 7.0, 9.0]));
        assert_eq!(contents(englang_tensor_mean_along(a, 1)), (vec![2], vec![2.0, 5.0]));
        assert_eq!(contents(unsafe { englang_tensor_mean(a) }), (vec![], vec![3.5]));
    }

    #[test]
    fn softmax_rows_add_up_to_one() {
        let a = tensor(&[2, 3], &[1.0, 2.0, 3.0, 1000.0, 1000.0, 1000.0]);
        let (_, data) = contents(unsafe { englang_tensor_softmax(a) });
        assert!((data[0..3].iter().sum::<f64>() - 1.0).abs() < 1e-12);
        assert!(data[2] > data[1] && data[1] > data[0]);
        assert!((data[3] - 1.0 / 3.0).abs() < 1e-12);
    }

    #[test]
    fn indexing_drops_the_first_dimension() {
        let a = tensor(&[2, 3], &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
        let row = unsafe { englang_tensor_index(a, 1) };
        assert_eq!(contents(row), (vec![3], vec![4.0, 5.0, 6.0]));
        assert_eq!(unsafe { englang_tensor_value(englang_tensor_index(row, 2)) }, 6.0);
    }

    #[test]
    fn nested_lists_become_tensors() {
        let rows = [from_slice(&[1.0f64.to_bits(), 2.0f64.to_bits()]), from_slice(&[3.0f64.to_bits(), 4.0f64.to_bits()])];
        let outer = from_slice(&rows.map(|p| p as u64));
        let t = unsafe { englang_tensor_from_list(outer, 2, 0) };
        assert_eq!(contents(t), (vec![2, 2], vec![1.0, 2.0, 3.0, 4.0]));
        let ints = from_slice(&[5i64 as u64, 6i64 as u64]);
        assert_eq!(contents(unsafe { englang_tensor_from_list(ints, 1, 1) }), (vec![2], vec![5.0, 6.0]));
    }

    #[test]
    fn random_tensors_look_like_a_bell_curve() {
        let dims = [100i64, 100];
        let (_, data) = contents(unsafe { englang_tensor_filled(2, dims.as_ptr(), 2) });
        let mean = data.iter().sum::<f64>() / data.len() as f64;
        let spread = (data.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / data.len() as f64).sqrt();
        assert!(mean.abs() < 0.05, "{mean}");
        assert!((spread - 1.0).abs() < 0.05, "{spread}");
    }

    #[test]
    fn shapes_read_like_english() {
        assert_eq!(with_article(&[3, 2]), "a 3-by-2 tensor");
        assert_eq!(with_article(&[8]), "an 8-number tensor");
        assert_eq!(with_article(&[]), "a single-number tensor");
    }
}
