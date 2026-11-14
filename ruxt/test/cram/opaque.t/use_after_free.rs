// Unsound: only found by running the drop of a generic `Box` to the end.
pub struct Raw<T> {
    p: *mut T,
}

pub fn new<T>(x: T) -> Raw<T> {
    Raw {
        p: Box::into_raw(Box::new(x)),
    }
}

pub fn read_after_free<T>(raw: Raw<T>) -> T {
    unsafe {
        drop(Box::from_raw(raw.p));
        std::ptr::read(raw.p)
    }
}
