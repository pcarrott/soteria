use std::ptr::NonNull;
pub struct R {
    p: NonNull<i32>,
}
pub fn new(x: i32) -> R {
    R {
        p: unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(x))) },
    }
}
pub fn peek(_w: &R) {}
