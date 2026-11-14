// Unsound: `b` is forgotten, whatever the size of `T`
pub struct S<T>(T);

pub fn new<T>(a: T, b: T) -> S<T> {
    std::mem::forget(b);
    S(a)
}
