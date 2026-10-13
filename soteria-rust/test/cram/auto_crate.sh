# Wrapper around `soteria-rust` for crate-based cram tests, stripping the
# nondeterministic bits of crate compilation so snapshots are reproducible.
test_auto_crate() {
  soteria-rust "$@" 2>&1 | sed -E \
    -e '/waiting for file lock on package cache/d' \
    -e 's/(lib[A-Za-z0-9_]+-)[0-9a-f]+\.rlib/\1HASH.rlib/g' \
    -e 's#(/build/[A-Za-z0-9_-]+/)[0-9a-f]{16}/#\1HASH/#g' \
    -e 's#(/target/)[a-z0-9_]+-[a-z0-9_]+-[a-z0-9_]+(-[a-z0-9_]+)?/#\1TRIPLE/#g'
  return "${PIPESTATUS[0]}"
}
