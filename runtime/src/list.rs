use std::ffi::c_void;

// Must match codegen: user code holds a pointer to the elements, and an
// {length, capacity} header of two i64s sits 16 bytes before it. Lists are
// malloc'd so codegen-emitted list operations can keep working on them.
const HEADER: usize = 16;

unsafe extern "C" {
    fn malloc(size: usize) -> *mut c_void;
}

pub unsafe fn as_slice<'a, T>(data: *const T) -> &'a [T] {
    unsafe {
        let len = *(data as *const u8).sub(HEADER).cast::<i64>();
        std::slice::from_raw_parts(data, len as usize)
    }
}

pub fn from_slice<T: Copy>(values: &[T]) -> *mut T {
    let len = values.len();
    unsafe {
        let base = malloc(HEADER + len * size_of::<T>()) as *mut u8;
        if base.is_null() {
            crate::fail("ran out of memory");
        }
        *base.cast::<i64>() = len as i64;
        *base.add(8).cast::<i64>() = len as i64;
        let data = base.add(HEADER).cast::<T>();
        std::ptr::copy_nonoverlapping(values.as_ptr(), data, len);
        data
    }
}
