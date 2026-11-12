// `Vec` and `Rc` free their contents through `Drop` impls of the standard library.
use std::rc::Rc;
pub struct S {
    v: Vec<i32>,
    r: Rc<i32>,
}
pub fn new(x: i32) -> S {
    let mut v = Vec::new();
    v.push(x);
    S { v, r: Rc::new(x) }
}
pub fn peek(_s: &S) {}
