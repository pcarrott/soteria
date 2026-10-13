pub fn has<T: PartialEq>(v: &[T], x: &T) -> bool {
    v.iter().any(|y| y == x)
}
fn main() {
    assert!(has(&[1u8, 2, 3], &2));
}
