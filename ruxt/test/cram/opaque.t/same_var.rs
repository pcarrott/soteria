// `Pair<T, T>` is a different type from `Pair<T, U>`: `same` cannot build the
// argument of `boom`.
pub struct Pair<T, U> {
    a: T,
    b: U,
}

pub fn same<T>(a: T, b: T) -> Pair<T, T> {
    Pair { a, b }
}

pub fn boom<A, B>(_p: Pair<A, B>) {
    let null = 0 as *const u8;
    unsafe {
        std::ptr::read(null);
    }
}
