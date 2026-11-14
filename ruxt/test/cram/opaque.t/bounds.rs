// Trait bounds are not supported: `boom` is skipped.
pub fn boom<T: Clone>(x: &T) -> T {
    x.clone()
}
