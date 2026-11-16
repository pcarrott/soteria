Test RUXt in a monomorphised version of the standard library's LinkedList
  $ ruxt linked_list.rs --only-public --pass-fuel 1
  Compiling... done in <time>
  No type unsoundness found!

  $ ruxt linked_list.rs --only-public --pass-fuel 2
  Compiling... done in <time>
  Found type unsoundness, witnessed by:
  #[test]
  fn witness() {
      let mut v5 = crate::LinkedList::new();
      let v4 = 0u32;
      <crate::LinkedList as core::iter::traits::collect::Extend<&u32>>::extend_one(&mut v5, &v4);
      std::mem::drop(v4);
      crate::LinkedList::cycle(&mut v5);
      std::mem::drop(v5);
  }
  [1]
