// Unsound: `x` is dropped twice, but `y` is never dropped
pub fn balanced<T>(x: T, y: T) {
    let z = unsafe { std::ptr::read(&x) };
    drop(z);
    drop(x);
    std::mem::forget(y);
}
