// A function whose only behaviour is to panic; called only on
// already-detected error paths.
// We shouldn't flag those as manifest panics.

fn panic_does_not_fit() -> ! {
    panic!("does not fit")
}

fn slice_index_order_fail() -> ! {
    unreachable!("slice index order fail")
}

fn assert_false() {
    assert!(false, "assert false")
}

// We check that this function is correctly flagged as a manifest panic.
fn real_manifest() {
    let x = soteria::nondet_bytes();
    if (x) {
        unreachable!(false);
    }
}
