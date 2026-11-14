// Unsound: `b` is forgotten only when `T` is not zero-sized, so the witness
// instantiates `T` with a type that is not zero-sized
pub struct S<T>(T);

pub fn new<T>(a: T, b: T) -> S<T> {
    if std::mem::size_of::<T>() == 0 {
        drop(b);
    } else {
        std::mem::forget(b);
    }
    S(a)
}
