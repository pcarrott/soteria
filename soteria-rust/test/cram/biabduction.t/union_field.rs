// A `#[repr(C)]` union with equally-sized POD variants is taken
// by value as a symbolic entry-point argument; reading a field of it
// triggers an "out of bounds" because the analyzer doesn't materialize
// the field layout inside the opaque union blob.
#[repr(C)]
union Vec128Storage {
    d: [u32; 4],
    q: [u64; 2],
}

fn read_d0(s: Vec128Storage) -> u32 {
    unsafe { s.d[0] }
}

fn read_q0(s: Vec128Storage) -> u64 {
    unsafe { s.q[0] }
}
