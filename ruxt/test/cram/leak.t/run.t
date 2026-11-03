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
