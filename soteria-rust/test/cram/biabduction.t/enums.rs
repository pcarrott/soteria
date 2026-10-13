fn unwrap(x: Result<(), ()>) {
    match x {
        Ok(()) => (),
        Err(()) => panic!(),
    }
}

fn wait_this_is_bad(x: Result<(), ()>) {
    if soteria::nondet_bytes() {
        panic!()
    }
}
