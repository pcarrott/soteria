#[soteria::test]
fn index_out_of_bounds() {
    let v = [1u32, 2, 3];
    let i: usize = soteria::nondet_bytes();
    let _ = v[i];
}

#[soteria::test]
fn get_unchecked_out_of_bounds() {
    let mut v = [1u32, 2, 3];
    let i: usize = soteria::nondet_bytes();
    let s: &mut [u32] = &mut v;
    unsafe { *s.get_unchecked_mut(i) = 0 };
}

#[soteria::test]
fn add_overflow() {
    let a: u8 = soteria::nondet_bytes();
    let _ = a + 1;
}

#[soteria::test]
fn div_by_zero() {
    let a: u8 = soteria::nondet_bytes();
    let b: u8 = soteria::nondet_bytes();
    let _ = a / b;
}

#[soteria::test]
fn neg_overflow() {
    let a: i8 = soteria::nondet_bytes();
    let _ = -a;
}

struct PanicOnDrop;
impl Drop for PanicOnDrop {
    fn drop(&mut self) {
        soteria::panic("dropped");
    }
}

// The box must be dropped while unwinding from the overflow, so the drop's panic
// terminates the unwind.
#[soteria::test]
fn failed_check_unwinds() {
    let _b = Box::new(PanicOnDrop);
    let a: u8 = soteria::nondet_bytes();
    let _ = a + 1;
}
