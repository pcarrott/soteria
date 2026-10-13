use std::mem::MaybeUninit;

pub struct H {
    x: u64,               // forces alignment > 1
    buf: MaybeUninit<u8>, // union field whose move triggers the crash
}

impl H {
    pub fn finalize(self: Box<Self>) -> MaybeUninit<u8> {
        self.buf
    }
}
