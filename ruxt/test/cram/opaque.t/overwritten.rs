// Unsound: `a` is overwritten without being dropped
pub struct S<T>(T);

pub fn new<T>(a: T, b: T) -> S<T> {
    let mut a = a;
    unsafe { std::ptr::write(&mut a, b) };
    S(a)
}
