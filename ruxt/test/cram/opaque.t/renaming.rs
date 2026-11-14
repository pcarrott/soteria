// `boom` is only reachable through a pair of two different type variables,
// named the other way around, as the value of `swap` and that of `pair` have the
// same summary.
pub struct Pair<T, U> {
    a: T,
    b: U,
}

pub fn pair<T, U>(a: T, b: U) -> Pair<T, U> {
    Pair { a, b }
}

pub fn swap<T, U>(p: Pair<T, U>) -> Pair<U, T> {
    Pair { a: p.b, b: p.a }
}

pub fn boom<B, A>(_p: Pair<A, B>) {
    let null = 0 as *const u8;
    unsafe {
        std::ptr::read(null);
    }
}
