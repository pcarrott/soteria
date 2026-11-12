// The raw pointer in `Cell` may be owned by a `Drop` impl we do not have, but
// the leak of the other box is still found.
use std::cell::Cell;
pub fn new(x: i32) -> Cell<*mut i32> {
    core::mem::forget(Box::new(1u32));
    Cell::new(Box::into_raw(Box::new(x)))
}
