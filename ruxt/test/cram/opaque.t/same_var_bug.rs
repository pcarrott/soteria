// `same` builds the argument of `boom`, whatever the name of its type variable.
// `pair` does not, as its type variables are different.
pub struct Pair<T, U> {
    a: T,
    b: U,
}

pub fn pair<T, U>(a: T, b: U) -> Pair<T, U> {
    Pair { a, b }
}

pub fn same<T>(a: T, b: T) -> Pair<T, T> {
    Pair { a, b }
}

pub fn boom<A>(_p: Pair<A, A>) {
    let null = 0 as *const u8;
    unsafe {
        std::ptr::read(null);
    }
}
