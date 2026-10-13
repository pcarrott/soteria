fn main() {
    let a = [1u32, 2, 3, 4];
    let x = unsafe { *a.get_unchecked(1) };
    assert!(x == 2);
}
