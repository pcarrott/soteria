open Charon
open Iarray.Infix
module LocalId = Expressions.LocalId

(** A forward, intraprocedural constant-propagation over [bool] locals. A local
    absent from the environment is [⊤] ("either", unknown); a present binding is
    a statically-known constant. [Bottom] is the unreachable environment (the
    join identity, used to seed not-yet-visited blocks). *)
type t = Bottom | Env of bool LocalId.Map.t [@@deriving show, eq]

module Analysis = Analysis.Make_forward (struct
  include Analysis.Domain.Default (struct
    (** A forward, intraprocedural constant-propagation over [bool] locals. A
        local absent from the environment is [⊤] ("either", unknown); a present
        binding is a statically-known constant. [Bottom] is the unreachable
        environment (the join identity, used to seed not-yet-visited blocks). *)
    type nonrec t = t = Bottom | Env of bool LocalId.Map.t

    let equal = equal
    let pp = pp
    let bottom = Bottom

    (** Least upper bound: keep only locals on which both environments agree. *)
    let join a b =
      match (a, b) with
      | Bottom, x | x, Bottom -> x
      | Env m1, Env m2 ->
          Env
            (LocalId.Map.merge
               (fun _ v1 v2 ->
                 match (v1, v2) with
                 | Some b1, Some b2 when Bool.equal b1 b2 -> Some b1
                 | _ -> None)
               m1 m2)

    (* On function entry no local is known to be a constant. *)
    let boundary = Env LocalId.Map.empty
  end)

  (* We only track [_l := const true] / [_l := const false]; any other write to
     a tracked local makes its value unknown again. *)
  let statement env (s : UllbcAst.statement) =
    match (env, s.kind) with
    | Env m, Assign ({ kind = PlaceLocal lid; _ }, op) -> (
        match op with
        | Use (Constant { kind = CLiteral (VBool b); _ }, _) ->
            Env (LocalId.Map.add lid b m)
        | Use (Copy { kind = PlaceLocal lid2; _ }, _retag) -> (
            (* Copying from another local: if we know the source local, we know
               the dest local. *)
            match LocalId.Map.find_opt lid2 m with
            | Some b -> Env (LocalId.Map.add lid b m)
            | None -> Env (LocalId.Map.remove lid m))
        | _ -> Env (LocalId.Map.remove lid m))
    | _ -> env
end)

let analyse_fun = Analysis.analyse_fun

let eval_bool_at fn block (op : Expressions.operand) : bool option =
  let env : Analysis.t = (analyse_fun fn).%(block) in
  match env with
  | Bottom -> None
  | Env m -> (
      match op with
      | Copy { kind = PlaceLocal lid; _ } | Move { kind = PlaceLocal lid; _ } ->
          LocalId.Map.find_opt lid m
      | Constant { kind = CLiteral (VBool b); _ } -> Some b
      | _ -> None)
