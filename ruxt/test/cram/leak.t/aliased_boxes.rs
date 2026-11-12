// Two boxes own the same block: dropping both is a double free.
pub struct T {
    a: Box<i32>,
    b: Box<i32>,
}
pub fn new(x: i32) -> T {
    let p = Box::into_raw(Box::new(x));
    unsafe {
        T {
            a: Box::from_raw(p),
            b: Box::from_raw(p),
        }
    }
}
pub fn peek(_w: &T) {}
