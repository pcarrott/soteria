use std::mem::ManuallyDrop;
pub struct S {
    b: ManuallyDrop<Box<i32>>,
}
pub fn new(x: i32) -> S {
    S {
        b: ManuallyDrop::new(Box::new(x)),
    }
}
pub fn peek(_s: &S) {}
