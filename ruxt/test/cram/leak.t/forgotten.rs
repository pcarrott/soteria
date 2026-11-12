// A value whose drop cannot be run is forgotten, and only dropped at the end of
// the witness, after everything the analysis ran.
pub struct S {
    v: Vec<i32>,
}
pub fn new() -> S {
    S { v: Vec::new() }
}
pub fn leak(s: &S) -> usize {
    Box::leak(Box::new(1));
    s.v.len()
}
