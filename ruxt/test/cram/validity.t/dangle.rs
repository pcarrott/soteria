// Unsound: the returned reference, nested in an `Option`, dangles.
pub fn dangle() -> Option<&'static u32> {
    let x = 5u32;
    let p = &x as *const u32;
    unsafe { Some(&*p) }
}
