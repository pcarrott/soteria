// A read through a returned reference to a Copy value copies it.
#[derive(Clone, Copy)]
pub struct P {
    a: i32,
}
pub struct S {
    x: i32,
    p: P,
}
pub fn new(x: i32) -> S {
    S { x, p: P { a: x } }
}
pub fn get(s: &S) -> &i32 {
    Box::leak(Box::new(1));
    &s.x
}
pub fn get_p(s: &S) -> &P {
    Box::leak(Box::new(1));
    &s.p
}
