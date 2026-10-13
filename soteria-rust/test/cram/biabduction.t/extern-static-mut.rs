// An `extern "C"` `static mut` makes the frontend ICE while type-checking the
// static. Minimised from libc (`mach_task_self_`).
extern "C" {
    pub static mut COUNTER: u32;
}
pub fn get() -> u32 {
    unsafe { COUNTER }
}
