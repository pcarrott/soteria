Test RUXt in toy_linked_list example
  $ ruxt safe_public.rs --only-public --pass-fuel 0
  Compiling... done in <time>
  No type unsoundness found!

  $ ruxt safe_public.rs --only-public --pass-fuel 1
  Compiling... done in <time>
  No type unsoundness found!

  $ ruxt safe_public.rs --only-public --pass-fuel 2
  Compiling... done in <time>
  Found type unsoundness!
  [1]

  $ ruxt safe_public.rs --only-public --pass-fuel 4
  Compiling... done in <time>
  Found type unsoundness!
  [1]
