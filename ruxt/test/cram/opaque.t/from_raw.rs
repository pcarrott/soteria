// Safe: dropping a `Box<B>` must use the type and the `Destruct` clause of `B`,
// not those of the generics of the `Box` impl.
pub struct Raw<T> {
    p: *mut T,
}

pub fn new<A, B>(_a: A, b: B) -> Raw<B> {
    Raw {
        p: Box::into_raw(Box::new(b)),
    }
}

pub fn free<A, B>(_a: A, raw: Raw<B>) {
    unsafe {
        drop(Box::from_raw(raw.p));
    }
}
