// A read through a returned reference to a value that is not Copy passes the
// reference to black_box, which checks that it may still be read.
pub struct W {
    v: Box<i32>,
}
pub struct S {
    w: W,
}
pub fn new(x: i32) -> S {
    S {
        w: W { v: Box::new(x) },
    }
}
pub fn get(s: &S) -> &W {
    Box::leak(Box::new(1));
    &s.w
}
