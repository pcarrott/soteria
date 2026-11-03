Test RUXt in a monomorphised version of the standard library's LinkedList
  $ ruxt linked_list.rs --only-public --pass-fuel 1
  Compiling... done in <time>
  No type unsoundness found!

  $ ruxt linked_list.rs --only-public --pass-fuel 2
  Compiling... done in <time>
  Found type unsoundness, witnessed by:
  #[test]
  fn witness() {
      let mut v20 = crate::LinkedList::new();
      crate::LinkedList::push_front(&mut v20, 0u32);
      crate::LinkedList::cycle(&mut v20);
      std::mem::drop(v20);
  }
  [1]
