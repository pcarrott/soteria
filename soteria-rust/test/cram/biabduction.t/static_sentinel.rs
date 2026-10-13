// The pointer to a fresh allocation is compared against the address of a
// `static` sentinel. Shouldn't reach the panic.
static SENTINEL: u8 = 0;

fn fresh_cannot_equal_sentinel() -> u8 {
    let local: u8 = 7;
    let p: *const u8 = &local;
    if p == &SENTINEL as *const u8 {
        panic!("infeasible: fresh local cannot alias the sentinel")
    }
    local
}
