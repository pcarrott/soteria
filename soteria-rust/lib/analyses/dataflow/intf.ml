open Charon

module Domain = struct
  module type Lattice = sig
    type t [@@deriving eq] [@@mixins Sigs.Printable]

    (** The least element [⊥]; the per-block seed and the value assumed for a
        recursive call back-edge. *)
    val bottom : t

    (** The fact at the analysis's source: function entry (forward) or the
        exit/return blocks (backward). *)
    val boundary : t

    (** Least upper bound, used to combine the facts on a block's in-edges;
        associative, commutative, idempotent. *)
    val join : t -> t -> t
  end

  module type S = sig
    include Lattice

    (** Transfer of a statement, mapping the joined in-edge facts to this
        block's fact. *)
    val statement : t -> UllbcAst.statement -> t

    (** Transfer of a block's terminator, mapping the joined in-edge facts to
        this block's fact. [summary] is the interprocedural summary of a call's
        callee, so a [Call] is handled here like any other terminator (e.g.
        [summary callee && input]) rather than being special-cased. *)
    val terminator :
      summary:(UllbcAst.call -> t) -> t -> UllbcAst.terminator_kind -> t

    (** The live successors of [block]'s terminator: lets an analysis prune
        statically-dead branches (e.g. a [Switch] on a known guard).
        [fn]/[block] identify the point so it may consult auxiliary analyses.
        The default is [successors_of] (no pruning). *)
    val live_successors :
      fn:Types.FunDeclId.id ->
      block:int ->
      UllbcAst.terminator_kind ->
      UllbcAst.BlockId.id list

    (** Summary assumed for a function whose body we do not analyse (builtins,
        trait/dyn dispatch, bodiless declarations). *)
    val opaque : UllbcAst.fun_decl -> t

    (** If [Some v], short-circuits a function to summary [v], ignoring its
        body. *)
    val known_summary : UllbcAst.fun_decl -> t option
  end
end

module type S = sig
  type t

  val analyse_fun : Types.FunDeclId.id -> t iarray
  val summary : Types.FunDeclId.id -> t
end
