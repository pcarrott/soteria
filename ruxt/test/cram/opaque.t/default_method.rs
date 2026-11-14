// A default trait method called on a generic impl. Safe.
pub trait Tr { fn v(&self) -> usize; fn d(&self) -> usize { self.v() } }
pub struct W<A, B> { a: A, b: B }
impl<A, B> Tr for W<A, B> { fn v(&self) -> usize { std::mem::size_of::<B>() } }
pub fn mk<X, Y>(a: X, b: Y) -> W<X, Y> { W { a, b } }
pub fn go<X, Y>(w: &W<X, Y>) {
    if w.d() != std::mem::size_of::<Y>() {
        let null = 0 as *const u8;
        unsafe { std::ptr::read(null); }
    }
}
