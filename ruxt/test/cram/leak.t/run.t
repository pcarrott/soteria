Memory leaks are reported separately from type unsoundness
  $ ruxt leak.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      crate::f();
  }
  [1]

  $ ruxt leak.rs --ignore-leaks
  Compiling... done in <time>
  No type unsoundness found!

Types without drop glue are dropped from their structure
  $ ruxt no_leak.rs
  Compiling... done in <time>
  No type unsoundness found!

Types of other crates may free their contents in a `Drop` impl we do not have
  $ ruxt foreign.rs
  Compiling... done in <time>
  No type unsoundness found!

These leak, as nothing frees the block
  $ ruxt raw_ptr.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      let v9 = crate::new(0i32);
      crate::peek(&v9);
      std::mem::drop(v9);
  }
  [1]
  $ ruxt non_null.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      let v9 = crate::new(0i32);
      crate::peek(&v9);
      std::mem::drop(v9);
  }
  [1]
  $ ruxt manually_drop.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      let v9 = crate::new(0i32);
      crate::peek(&v9);
      std::mem::drop(v9);
  }
  [1]
  $ ruxt noop_drop.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      let v9 = crate::new(0i32);
      crate::peek(&v9);
      std::mem::drop(v9);
  }
  [1]

Dropping from the structure still finds errors
  $ ruxt aliased_boxes.rs
  Compiling... done in <time>
  Found type unsoundness, witnessed by:
  #[test]
  fn witness() {
      let v13 = crate::new(0i32);
      crate::peek(&v13);
      std::mem::drop(v13);
  }
  [1]

A leak is still found next to a type of another crate
  $ ruxt foreign_leak.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      crate::new(0i32);
  }
  [1]

The contents of a box are dropped before the box is freed
  $ ruxt box_contents.rs
  Compiling... done in <time>
  No type unsoundness found!

Values whose drop cannot be run are only dropped at the end of the witness
  $ ruxt forgotten.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      let v8 = crate::new();
      crate::leak(&v8);
      let _v22 = v8;
  }
  [1]

Reads through returned references are safe code: a copy for Copy values, and a
retag otherwise
  $ ruxt read_copy.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      let v5 = crate::new(0i32);
      let v1 = crate::get_p(&v5);
      let _v16 = *v1;
      std::mem::drop(v5);
  }
  [1]
  $ ruxt read_ref.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      let v9 = crate::new(0i32);
      let v1 = crate::get(&v9);
      std::hint::black_box(v1);
      std::mem::drop(v9);
  }
  [1]

Writes through returned mutable references replace the old value
  $ ruxt write.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      let mut v10 = crate::new();
      let v1 = crate::get(&mut v10);
      let v26 = crate::mk();
      std::mem::drop(std::mem::replace(v1, v26));
      std::mem::drop(v10);
  }
  [1]
