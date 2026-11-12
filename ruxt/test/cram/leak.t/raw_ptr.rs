pub struct R {
    p: *mut i32,
}
pub fn new(x: i32) -> R {
    R {
        p: Box::into_raw(Box::new(x)),
    }
}
pub fn peek(_w: &R) {}
