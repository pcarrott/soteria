// Unsound: the argument is dropped twice, once as a copy and once at the end of
// its scope.
pub fn double_drop<T>(x: T) {
    let y = unsafe { std::ptr::read(&x) };
    drop(y);
}
