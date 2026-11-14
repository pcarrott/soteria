Test RUXt with opaque summaries of type variables
  $ ruxt renaming.rs
  Compiling... done in <time>
  Found type unsoundness, witnessed by:
  #[test]
  fn witness() {
      struct T0;
      struct T1;
      let v8 = crate::pair(T1, T0);
      crate::boom(v8);
  }
  [1]
  $ ruxt same_var.rs
  Compiling... done in <time>
  No type unsoundness found!
  $ ruxt same_var_bug.rs
  Compiling... done in <time>
  Found type unsoundness, witnessed by:
  #[test]
  fn witness() {
      struct T0;
      let v7 = crate::same(T0, T0);
      crate::boom(v7);
  }
  [1]
  $ ruxt heap.rs --pass-fuel 2
  Compiling... done in <time>
  No type unsoundness found!
  $ ruxt bounds.rs
  Compiling... done in <time>
  No type unsoundness found!
  $ ruxt from_raw.rs --ignore-leaks
  Compiling... done in <time>
  No type unsoundness found!
  $ ruxt use_after_free.rs --ignore-leaks
  Compiling... done in <time>
  Found type unsoundness, witnessed by:
  #[test]
  fn witness() {
      struct T0([u8; 1]);
      let v17 = crate::new(T0([0; 1]));
      crate::read_after_free(v17);
  }
  [1]
  $ ruxt default_method.rs --ignore-leaks
  Compiling... done in <time>
  No type unsoundness found!
  $ ruxt double_drop.rs
  Compiling... done in <time>
  Found type unsoundness, witnessed by:
  #[test]
  fn witness() {
      struct T0;
      crate::double_drop(T0);
  }
  [1]
  $ ruxt undropped.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      struct T0;
      crate::forget(T0);
  }
  [1]
  $ ruxt dropped_once.rs
  Compiling... done in <time>
  No type unsoundness found!
  $ ruxt balanced.rs
  Compiling... done in <time>
  Found type unsoundness, witnessed by:
  #[test]
  fn witness() {
      struct T0;
      crate::balanced(T0, T0);
  }
  [1]
  $ ruxt freed.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      struct T0([u8; 1]);
      crate::new(T0([0; 1]), T0([0; 1]));
  }
  [1]
  $ ruxt overwritten.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      struct T0([u8; 1]);
      crate::new(T0([0; 1]), T0([0; 1]));
  }
  [1]
  $ ruxt forgotten.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      struct T0;
      crate::new(T0, T0);
  }
  [1]
  $ ruxt sized_leak.rs
  Compiling... done in <time>
  Found memory leak, witnessed by:
  #[test]
  fn witness() {
      struct T0([u8; 1]);
      crate::new(T0([0; 1]), T0([0; 1]));
  }
  [1]
