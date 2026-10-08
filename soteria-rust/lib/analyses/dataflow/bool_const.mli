open Charon

(** A forward, intraprocedural constant-propagation over [bool] locals: it
    tracks locals statically known to equal [true] or [false] (from
    [_l := const b] assignments), so that a branch on such a local can be
    resolved. *)
type t

(** [analyse_fun fn] is the known-constant environment at the {e exit} of each
    block of [fn]'s body (i.e. what the block's terminator observes), indexed by
    {!Charon.UllbcAst.BlockId.to_int}, memoised across calls. Empty for a
    function without an unstructured body. *)
val analyse_fun : Types.FunDeclId.id -> t iarray

(** [eval_bool env op] is the statically-known boolean value of [op] under
    [env], or [None] if it could be either. *)
val eval_bool_at :
  Types.FunDeclId.id -> int -> Expressions.operand -> bool option
