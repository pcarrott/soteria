// A constructor allocates an `Arc`/`Box` and returns it (directly
// or transitively in a returned struct). This currently causes a false positive
// leak detection in bi-abduction
extern crate alloc;
use alloc::boxed::Box;
use alloc::sync::Arc;

struct Inner {
    x: i32,
}

struct Holder {
    inner: Arc<Inner>,
}

fn new_holder() -> Holder {
    Holder {
        inner: Arc::new(Inner { x: 42 }),
    }
}

fn new_boxed() -> Box<Inner> {
    Box::new(Inner { x: 7 })
}
