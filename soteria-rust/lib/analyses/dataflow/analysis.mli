open Charon

(** Abstract domain for dataflow analyses over the ULLBC supergraph. The domain
    describes only a lattice and per-instruction transfers (a transfer is just
    [input fact -> output fact] and never sees the graph);
    {!Make_forward}/{!Make_backward} solve it by a worklist least fixpoint.

    The {e only} difference between forward and backward analysis is which edges
    feed a block's input — its predecessors (forward) or its (live) successors
    (backward) — and the direction the transfer is threaded.

    This code is inspired by very standard dataflow analysis, according to
    {{:https://cs.au.dk/~amoeller/spa/spa.pdf}many textbooks}. *)
module Domain : sig
  module type Lattice = Intf.Domain.Lattice
  module type S = Intf.Domain.S

  module Default (L : Lattice) : S with type t = L.t
end

(** The CFG successors of a terminator, including unwind/secondary targets; the
    default (no-pruning) {!Domain.S.live_successors}. *)
val successors_of : UllbcAst.terminator_kind -> UllbcAst.BlockId.id list

module type S = Intf.S

(** Solve an analysis forwards: a block's input joins its predecessors' facts
    (and the {!Domain.S.boundary} at the entry). *)
module Make_forward (D : Domain.S) : S with type t = D.t

(** Solve an analysis backwards: a block's input joins its (live) successors'
    facts (and the {!Domain.S.boundary} at the exits). *)
module Make_backward (D : Domain.S) : S with type t = D.t
