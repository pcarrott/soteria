// Unsound: the argument is never dropped, and the unit value returned cannot
// hold it.
pub fn forget<T>(x: T) {
    let _ = std::mem::ManuallyDrop::new(x);
}
