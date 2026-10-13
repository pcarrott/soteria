  $ soteria-rust auto simple.rs
  Compiling... done in <time>
  
  error: found issues in <time>
  error: Panic: aha! in simple::half_manifest
      --> $TESTCASE_ROOT/simple.rs:15:9
   11 |  fn half_manifest() -> i32 {
      |  ------------------------- 1: Entry point
      .  
   15 |          panic!("aha!");
      |          ^^^^^^^^^^^^^^
      |          |
      |          Triggering operation
      |          2: Call trace
  
  error: Panic: attempt to divide `V|1|` by zero in simple::manifest
      --> $TESTCASE_ROOT/simple.rs:7:5
    5 |  fn manifest(x: i32) -> i32 {
      |  -------------------------- 1: Entry point
    6 |      let mut z = x;
    7 |      z /= z - x; // always a division by zero
      |      ^^^^^^^^^^ Triggering operation
  
  [1]

Test some bi-abduction with references.
  $ soteria-rust auto refs.rs
  Compiling... done in <time>
  
  error: found issues in <time>
  error: Panic: tough luck in refs::coinflip
      --> $TESTCASE_ROOT/refs.rs:9:9
    1 |  fn coinflip(b: &bool) -> i32 {
      |  ---------------------------- 1: Entry point
      .  
    9 |          panic!("tough luck");
      |          ^^^^^^^^^^^^^^^^^^^^
      |          |
      |          Triggering operation
      |          2: Call trace
  
  error: Panic: tough luck again! in refs::coinflip_but_struct
      --> $TESTCASE_ROOT/refs.rs:29:9
   19 |  fn coinflip_but_struct(f1: &Foo, f2: &Foo) -> i32 {
      |  ------------------------------------------------- 1: Entry point
      .  
   29 |          panic!("tough luck again!");
      |          ^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |          |
      |          Triggering operation
      |          2: Call trace
  
  [1]

  $ soteria-rust auto panic_helper.rs
  Compiling... done in <time>
  
  error: found issues in <time>
  error: Panic in panic_helper::real_manifest
      --> $RUSTLIB/library/core/src/panicking.rs:240:5
  240 |      panic_fmt(format_args!("internal error: entered unreachable code: {}", *x));
      |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |      |
      |      Triggering operation
      |      3: Call trace
      --> $TESTCASE_ROOT/panic_helper.rs:21:9
   18 |  fn real_manifest() {
      |  ------------------ 1: Entry point
      .  
   21 |          unreachable!(false);
      |          ------------------- 2: Call trace
  
  [1]

  $ soteria-rust auto static_sentinel.rs
  Compiling... done in <time>
  
  note: No bugs found

  $ soteria-rust auto union_field.rs
  Compiling... done in <time>
  
  note: No bugs found

  $ soteria-rust auto construct_arc.rs
  Compiling... done in <time>
  
  note: No bugs found

Test bi-abduction with enums.
  $ soteria-rust auto enums.rs
  Compiling... done in <time>
  
  error: found issues in <time>
  error: Panic: explicit panic in enums::wait_this_is_bad
      --> $TESTCASE_ROOT/enums.rs:10:9
    8 |  fn wait_this_is_bad(x: Result<(), ()>) {
      |  -------------------------------------- 1: Entry point
    9 |      if soteria::nondet_bytes() {
   10 |          panic!()
      |          ^^^^^^^^
      |          |
      |          Triggering operation
      |          2: Call trace
  
  [1]
Test that leaks are propery detected.
  $ soteria-rust auto leak.rs
  Compiling... done in <time>
  
  error: found issues in <time>
  warning: Memory leak in leak::some_function
      --> $RUSTLIB/library/alloc/src/alloc.rs:131:9
  131 |            __rust_alloc(layout.size(), layout.alignment())
      |            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |            |
      |            Triggering operation
      |            8: Allocation
      .    
  308 |                    let raw_ptr = if zeroed { alloc_zeroed(layout) } else { alloc(layout) };
      |                                                                            ------------- 7: Call trace
      .    
  429 |        const fn alloc_impl(&self, layout: Layout, zeroed: bool) -> Result<NonNull<[u8]>, AllocError> {
  430 | /          core::intrinsics::const_eval_select(
  431 | |              (layout, zeroed),
  432 | |              Global::alloc_impl_const,
  433 | |              Global::alloc_impl_runtime,
  434 | |          )
      | \----------' 6: Call trace
  435 |        }
      .    
  548 |            self.alloc_impl(layout, false)
      |            ------------------------------ 5: Call trace
      --> $RUSTLIB/library/alloc/src/boxed.rs:292:19
  251 |        match Global.allocate(layout) {
      |              ----------------------- 4: Call trace
      .    
  292 |            let ptr = box_new_uninit(<T as SizedTypeProperties>::LAYOUT) as *mut T;
      |                      -------------------------------------------------- 3: Call trace
      --> $TESTCASE_ROOT/leak.rs:2:22
    1 |    fn some_function() {
      |    ------------------ 1: Entry point
    2 |        std::mem::forget(Box::new(11));
      |                         ------------ 2: Call trace
  
  [1]

  $ soteria-rust auto global_no_leak.rs
  Compiling... done in <time>
  
  note: No bugs found

An `extern "C"` `static mut`.
  $ soteria-rust auto extern-static-mut.rs 2>&1 | sed -E "s/'rustc' \([0-9]+\)/'rustc' (PID)/"
  Compiling... done in <time>
  
  note: No bugs found

This is a false positive
  $ soteria-rust auto keep_decay.rs
  Compiling... done in <time>
  
  note: No bugs found
  $ soteria-rust auto needs_fix.rs --stats stdout
  Compiling... done in <time>
  
  Statistics:
  • Z3 check-sat calls: 4
  • branch_on: branches 25% of calls (7 of 28)
  • Execution time: <time>
  • Steps: 3
  • Function calls: 2
  • Load accesses: 9 (88.89% through store)
  • SAT checks: 17 (0 unknowns)
  • SAT solving time: <time> (<%>)
  • Branches: 6 (2 unexplored)
  
  note: No bugs found

  $ soteria-rust auto struct_maybe_uninit.rs
  Compiling... done in <time>
  
  note: No bugs found
