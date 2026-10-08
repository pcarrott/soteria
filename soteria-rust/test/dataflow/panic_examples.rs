pub fn ok() {}

pub fn do_panic() {
    panic!()
}

// A function whose only action is to call another function that panics.
pub fn calls_panic() {
    do_panic()
}

pub enum Choice {
    A,
    B,
}

fn panic_a() {
    panic!("error A")
}

fn panic_b() {
    panic!("error B")
}

// Pattern matches on an enum, and on each variant calls a sub-function that
// panics with a different error message.
pub fn match_panic(choice: Choice) {
    match choice {
        Choice::A => panic_a(),
        Choice::B => panic_b(),
    }
}

// Two mutually recursive functions that inevitably panic at some point.
pub fn ping(n: u32) {
    if n == 0 {
        panic!("ping reached the base case")
    } else {
        pong(n - 1)
    }
}

pub fn pong(n: u32) {
    if n == 0 {
        panic!("pong reached the base case")
    } else {
        ping(n - 1)
    }
}

// An intra-function infinite loop: never returns, never panics, but is still a
// "panic helper" under the "never returns normally" semantics.
pub fn never() -> ! {
    loop {}
}

pub fn some_panic(x: u32) -> u32 {
    if (x > 0) {
        return 0;
    } else {
        panic!()
    }
}

pub fn call_some_panic_or_panic(x: u32) {
    if (x > 0) {
        some_panic(x);
    } else {
        panic!()
    }
}

pub fn call_some_panic_or_never(x: u32) {
    if (x > 0) {
        some_panic(x);
    } else {
        never();
    }
}

pub fn call_some_panic_or_return(x: u32) {
    if (x > 0) {
        some_panic(x);
    } else {
        return;
    }
}

pub fn call_some_panic(x: u32) {
    some_panic(x);
}

// Self-recursion with a base case: can return.
pub fn count_down(n: u32) -> u32 {
    if n == 0 {
        0
    } else {
        count_down(n - 1)
    }
}

// Direct self-recursion with no base case: diverges.
pub fn spin(n: u32) -> u32 {
    spin(n + 1)
}

// Mutually recursive, each with a base case that returns.
pub fn even(n: u32) -> bool {
    if n == 0 {
        true
    } else {
        odd(n - 1)
    }
}

pub fn odd(n: u32) -> bool {
    if n == 0 {
        false
    } else {
        even(n - 1)
    }
}

// An intra-function loop with an exit: returns.
pub fn loop_with_break(n: u32) -> u32 {
    let mut i = 0;
    loop {
        if i >= n {
            break;
        }
        i += 1;
    }
    i
}

// Diverges through a non-panic, never-returning library function.
pub fn calls_exit() {
    std::process::exit(0)
}

pub fn panic_and_continue() {
    panic!();
    return;
}

fn slice_index_order_fail() -> ! {
    unreachable!("slice index order fail")
}

fn assert_false() {
    assert!(false, "assert false")
}

fn debug_assert_false() {
    debug_assert!(false)
}

fn returns_unit() {}

pub fn calls_returning_fn() {
    returns_unit();
}

pub fn panic_helper_or_return(x: u32) -> u32 {
    if x == 0 {
        panic_a();
    }
    x
}

pub fn maybe_panic_in_loop(n: u32) -> u32 {
    let mut i = 0;
    while i < n {
        if i == 100 {
            panic!("overflow");
        }
        i += 1;
    }
    i
}

pub fn partial_match_panic(choice: Choice) -> u32 {
    match choice {
        Choice::A => panic!("only A panics"),
        Choice::B => 1,
    }
}

pub fn propagate() {
    let t = true;
    if (t) {
        panic!()
    }
}
