pub fn f(a: u32, b: u32) {
    if a != b {
        unsafe { core::hint::unreachable_unchecked() }
    }
}
