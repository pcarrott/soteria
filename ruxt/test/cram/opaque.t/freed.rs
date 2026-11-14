// Unsound: `b` is moved to the heap, which is freed without dropping it
use std::alloc::{dealloc, Layout};

pub struct S<T>(T);

pub fn new<T>(a: T, b: T) -> S<T> {
    let p = Box::into_raw(Box::new(b));
    if std::mem::size_of::<T>() != 0 {
        unsafe { dealloc(p as *mut u8, Layout::new::<T>()) };
    }
    S(a)
}
