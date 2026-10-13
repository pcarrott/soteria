// This tests exercises the idea of keeping track of a pointer through its
// address. Because we don't keep track of the pointer, without care,
// it would be detected as a leak.

pub fn init(x: u64) -> usize {
    Box::into_raw(Box::new(x)) as usize
}

pub unsafe fn free(ptr: usize) {
    drop(Box::from_raw(ptr as *mut u64));
}
