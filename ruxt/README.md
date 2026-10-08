# RUXt

RUXt is a type-safety refuter for Rust libraries, built on Soteria Rust It
searches for a sequence of calls to a library's **safe** API that reaches
undefined behaviour: such a sequence shows that the library's use of `unsafe`
is unsound.

```bash
dune exec -- ruxt <file.rs | crate-dir> [--only-public] [--pass-fuel N]
```

It prints `Found type unsoundness!` (exit code 1), `Found memory leak!`
(exit code 1) or `No type unsoundness found!` (exit code 0).

## How it works

RUXt computes, for each type `T` of the library, an under-approximation of the
values of `T` that safe code can build. It does so by repeatedly calling the
library's functions on the values found so far. Any call that goes wrong along
the way is a type unsoundness.

**Library** (`library.ml`). The local, safe functions of the crate (only the
public ones with `--only-public`). Functions whose arguments are all primitive
types are *constructors*. `Drop` impls are recorded to drop values.

**Summaries** (`summary.ml`). A summary `{ret; asrt}` of type `T` describes a
value `r` of type `T` and the heap it owns:

```
∃x⃗. ⌜π⌝ ∗ ℓ₁ ↦ b₁ ∗ … ∗ ℓₙ ↦ bₙ
```

Only the heap and path condition reachable from `r` are kept. A heap
allocation that is still alive but unreachable from `r` is a memory leak.

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

The other runs of `even.t` make `succ` private, or `unsafe`, so that it is not
part of the analysed API. Then `next` only produces values implied by `Σ_new`,
no new summary is found after pass 1, and RUXt reports no unsoundness.
