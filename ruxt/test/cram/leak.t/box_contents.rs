// The contents of a box are dropped in place, before the box is freed, so
// their drop can still read the rest of the box.
pub struct Inner {
    back: *const i32,
}
impl Drop for Inner {
    fn drop(&mut self) {
        unsafe { assert!(*self.back >= 0 || *self.back < 0) }
    }
}
pub struct Outer {
    b: Box<(i32, Inner)>,
}
pub fn new(x: i32) -> Outer {
    let raw = Box::into_raw(Box::new((
        x,
        Inner {
            back: std::ptr::null(),
        },
    )));
    unsafe {
        (*raw).1.back = &raw const (*raw).0;
        Outer {
            b: Box::from_raw(raw),
        }
    }
}
pub fn peek(_o: &Outer) {}
