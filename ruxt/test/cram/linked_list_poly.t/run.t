Test RUXt in the standard library's LinkedList
  $ ruxt linked_list.rs --only-public --pass-fuel 1
  Compiling... done in <time>
  No type unsoundness found!

  $ ruxt linked_list.rs --only-public --pass-fuel 2
  Compiling... done in <time>
  Found type unsoundness, witnessed by:
  #[test]
  fn witness() {
      struct T0;
      let mut v21 = crate::LinkedList::<_>::new();
      crate::LinkedList::<_>::push_front(&mut v21, T0);
      crate::LinkedList::<_>::cycle(&mut v21);
      std::mem::drop(v21);
  }
  [1]
