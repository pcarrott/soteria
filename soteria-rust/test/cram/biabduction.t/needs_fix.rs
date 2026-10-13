pub fn f(p: *const u8) -> u8 {
    unsafe { *p.add(1) }
}
