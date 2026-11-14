# RUXt

RUXt is a type-safety refuter for Rust libraries, built on Soteria Rust It
searches for a sequence of calls to a library's **safe** API that reaches
undefined behaviour: such a sequence shows that the library's use of `unsafe`
is unsound.

```bash
dune exec -- ruxt <file.rs | crate-dir> [--only-public] [--pass-fuel N]
```

It prints `No type unsoundness found!` (exit code 0), or a type unsoundness or
memory leak together with a witness (exit code 1): a Rust test that reaches the
bug through the safe API alone, to be added to the root of the crate.

## How it works

RUXt computes, for each type `T` of the library, an under-approximation of the
values of `T` that safe code can build. It does so by repeatedly calling the
library's functions on the values found so far. Any call that goes wrong along
the way is a type unsoundness.

**Library** (`library.ml`). The local, safe functions of the crate (only the
public ones with `--only-public`). Functions whose arguments are all primitive
types are *constructors*. `Drop` impls are recorded to drop values.

**Summaries** (`summary.ml`). A summary `{ret; asrt; witness}` of type `T`
describes a value `r` of type `T` and the heap it owns:

```
∃x⃗. ⌜π⌝ ∗ ℓ₁ ↦ b₁ ∗ … ∗ ℓₙ ↦ bₙ
```

Only the heap and path condition reachable from `r` and from the symbolic values
of the witness are kept. A heap allocation that is still alive but unreachable
is a memory leak.

**Witnesses** (`witness.ml`). The witness of a summary is a Rust expression
building `r` through calls to the library, parametric on symbolic primitive
values. The witness of a call runs the witnesses of its arguments, calls the
function and drops what the call drops. Its variables are symbolic variables,
printed as Rust variables: producing a summary renames them to fresh ones, so
composing witnesses needs no renaming. When a call goes wrong, its witness is
instantiated with a model of the path condition of the failing path.

A witness is safe Rust that reads every value the analysis reads, so a bug it
reaches lies in the library. A reference argument is moved out by
`std::mem::drop` after the call, and a value whose drop cannot be run is moved
to a variable that is only dropped at the end of the test. A returned `&mut`
is written with `std::mem::replace`. A returned `&` is read by copying the
value if it is `Copy`; otherwise the reference is passed to
`std::hint::black_box`, which checks that it may still be read without reading
the value itself.

**Passes** (`library.ml`, `refute.ml`). Pass 0 runs the constructors.
Passes 1 to `--pass-fuel` (default 5) run every other function on each
combination of argument summaries that uses at least one summary found in the
previous pass. Primitive arguments get fresh symbolic values. Summaries found
during a pass are only used from the next pass on.

**Calls** (`wrapper.ml`). For each call:

1. The argument summaries are produced into an empty state. Each reference
   argument becomes a fresh heap cell holding its value.
2. The function body is executed symbolically, with callees inlined.
3. Each successful path gives one summary for the return value, plus one
   summary per reference argument (the value left behind the reference).
   Everything else is dropped, running `Drop` impls.
4. Any path that does not terminate successfully is a type unsoundness.

**Drops** (`drop.ml`). Charon only translates the drop glue of the types that
the crate itself drops. For other types, RUXt runs the drop from their structure,
in place like `std::ptr::drop_in_place`: their `Drop` impl if any, then their
fields, where a `Box` drops its contents before freeing its block, and a raw
pointer owns nothing. A type of another crate that holds
a pointer, like `Vec`, may own it through a `Drop` impl that was not translated: it
is forgotten instead, and the blocks reachable from it are not reported as leaks.

**Type variables** (`summary.ml`, `charon_ext/`). A function argument whose type
is a type variable `T` gets an opaque summary, which owns a value of `T`.
Summaries are stored by type up to a one-to-one renaming of type variables:
`Pair<T, U>` and `Pair<U, T>` share their summaries, unlike `Pair<T, T>`. They
are renamed to the type variables of the function they are passed to. The
layouts of the type variables are part of the state of a summary. An owned
value of `T` is a memory leak if it is unreachable and `T` is not zero-sized on
that path, or if the type of the summary does not mention `T`. Type variables
with trait bounds other than the implicit `Destruct`, and const generics, are
not supported: such functions are skipped. In a witness, a type variable is
instantiated with a local struct: a unit struct when it may be zero-sized, or
else a struct of the size of its layout in a model of the path condition.

**Subsumption** (`Summary.Context.stage`). A new summary is discarded if it
implies an existing summary of the same type, and existing summaries implied
by the new one are removed.

## Example: `test/cram/even.t`

```rust
pub struct Even { value: i32 }

pub fn zero() -> Even { Even { value: 0 } }
pub fn new(n: i32) -> Even { Even { value: n - n % 2 } }

pub fn succ(x: &mut Even) {            // breaks evenness
    if (*x).value < i32::MAX - 1 { (*x).value += 1 }
}
pub fn next(x: &mut Even) { succ(x); succ(x) }

pub fn noop(x: Even) {                 // in bounds only if x.value is even
    let mut value = x.value;
    let ofs = (value % 2) as isize;
    let p = &raw mut value;
    unsafe { *p.offset(ofs) = value }
}
```

On `unsafe_public.rs --only-public` (above), the summaries of `Even` evolve as
follows. Their heap part is `emp`, as `Even` owns no heap.

| Pass | Call     | Summary of the output                                             | Outcome                       |
| ---- | -------- | ------------------------------------------------------------------| ----------------------------- |
| 0    | `new(n)` | `Σ_new ≜ ∃n. ⌜r = Even{n − n rem 2}⌝`                           | kept                          |
| 0    | `zero()` | `⌜r = Even{0}⌝`                                                 | implies `Σ_new`: discarded    |
| 1    | `noop`   | on `Σ_new`: `ofs = 0`, the write is in bounds                     | –                             |
| 1    | `next`   | `Even{v+2}` or `Even{v}`, with `v` from `Σ_new`                   | implies `Σ_new`: discarded    |
| 1    | `succ`   | `Σ_succ ≜ ∃n,v. ⌜r = Even{v+1} ∧ v = n − n rem 2 ∧ v <s MAX−1⌝` | kept                          |
| 2    | `noop`   | on `Σ_succ`: `ofs ≠ 0`, out-of-bounds store                       | **type unsoundness**          |

The witness of the type unsoundness composes the witnesses of `Σ_new` and
`Σ_succ` with the call to `noop`, at a model where `n = -1`:

```rust
#[test]
fn witness() {
    let mut v5 = crate::new(-1i32);
    crate::succ(&mut v5);
    crate::noop(v5);
}
```

The other runs of `even.t` make `succ` private, or `unsafe`, so that it is not
part of the analysed API. Then `next` only produces values implied by `Σ_new`,
no new summary is found after pass 1, and RUXt reports no unsoundness.
