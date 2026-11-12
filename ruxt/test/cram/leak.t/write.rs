// A write through a returned mutable reference replaces the old value, which
// is dropped.
pub struct W {
    v: Box<i32>,
}
pub struct S {
    x: Box<W>,
}
pub fn mk() -> W {
    W { v: Box::new(1) }
}
pub fn new() -> S {
    S {
        x: Box::new(W { v: Box::new(0) }),
    }
}
pub fn get(s: &mut S) -> &mut W {
    let p = &mut *s.x as *mut W;
    unsafe {
        std::ptr::write(p, W { v: Box::new(2) });
        &mut *p
    }
}
