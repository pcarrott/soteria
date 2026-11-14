// Sound: every argument is either dropped once or returned.
pub fn consume<T>(x: T) {
    drop(x);
}

pub fn identity<T>(x: T) -> T {
    x
}

pub fn first<T, U>(x: T, _y: U) -> T {
    x
}
