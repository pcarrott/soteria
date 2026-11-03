Arguments are produced independently from each other
  $ ruxt two_ints.rs
  Compiling... done in <time>
  Found type unsoundness, witnessed by:
  #[test]
  fn witness() {
      crate::f(4294967295u32, 0u32);
  }
  [1]
