open Charon

let known_panic_helpers =
  [
    "core::panicking::panic";
    "core::panicking::panic_fmt";
    "core::panicking::panic_display";
    "std::rt::begin_panic";
    "std::rt::panic_display";
    "std::rt::panic_fmt";
  ]

module NameMatcherMap = Charon.NameMatcher.NameMatcherMap

let match_config =
  NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

let matcher =
  NameMatcherMap.of_list
    (List.map
       (fun name -> (NameMatcher.parse_pattern name, ()))
       known_panic_helpers)

let is_known_panic_helper (name : Types.name) =
  let ctx = Crate.as_namematcher_ctx () in
  NameMatcherMap.find_opt ctx match_config name matcher |> Option.is_some

let ty_is_never : Types.ty -> bool = function TNever -> true | _ -> false

(** Panic-helper detection as an abstract interpretation. The abstract value of
    a program point is [true] iff {e some} execution from there reaches a normal
    [Return]; a function is a panic helper exactly when its entry cannot, so
    {!is_never} negates the entry summary.

    Phrased this way it is an ordinary least fixpoint over [bool] ordered
    [false ⊑ true] with [⊥ = false] ("no returning path (yet)") and
    [join = (||)], so intra-function loops converge to "cannot return" without a
    separate greatest-fixpoint seed. *)
module Never : Analysis.Domain.S with type t = bool = struct
  type t = bool [@@deriving show, eq]

  let bottom = false
  let boundary = false
  let join = ( || )

  (* Statements don't change whether a [Return] is reachable. *)
  let statement t _ = t

  (* The transfer is the identity on its joined successors, except at the
     terminators that decide reachability outright. A call's continuation
     reaches a return only if the callee returns {e and} the continuation then
     reaches one; the unwind successor folded into [input] cannot reach a
     return, so it contributes nothing. *)
  let terminator ~summary input (kind : UllbcAst.terminator_kind) =
    match kind with
    | Return -> true
    (* Unwinding terminators diverge: they never reach a normal return. *)
    | Abort _ | UnwindResume -> false
    | Call (c, _, _) ->
        (* If call diverges, then the entire thing diverges *)
        summary c && input
    | _ -> input

  let live_successors ~fn ~block (kind : UllbcAst.terminator_kind) =
    match kind with
    | Switch (op, If (b1, b2)) -> (
        (* If the guard is a statically-known boolean, only one branch is
           live. *)
        match Bool_const.eval_bool_at fn block op with
        | Some true -> [ b1 ]
        | Some false -> [ b2 ]
        | None -> [ b1; b2 ])
    (* For everything else the default CFG successors are fine: a [Drop] or
       [TAssert] is assumed to succeed (its unwind edge cannot reach a return,
       so joining it is harmless), and [Call] is handled by {!terminator}. *)
    | _ -> Analysis.successors_of kind

  (* Panic helpers and never-typed functions can never reach a return. *)
  let known_summary (decl : UllbcAst.fun_decl) =
    if
      ty_is_never decl.signature.output
      || is_known_panic_helper decl.item_meta.name
    then Some false
    else None

  (* A function we don't descend into can return unless its type says it
     can't. *)
  let opaque (decl : UllbcAst.fun_decl) =
    not (ty_is_never decl.signature.output)
end

include Analysis.Make_backward (Never)

let is_never f = not (summary f)
