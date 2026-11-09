(** Opaque predicates: a value of a type variable is opaque, and is owned by the
    state, by type variable. Dropping it moves it from the owned values to the
    dropped values of its type variable. *)

open Svalue

type 'a opaque = 'a list * 'a list

let pp_opaque pp_a ft (owned, dropped) =
  Fmt.pf ft "Opaque(%a, %a)" (Fmt.list pp_a) owned (Fmt.list pp_a) dropped

module Value = struct
  include Typed

  type t = T.any Typed.t
  type syn = Expr.t [@@deriving show]

  let to_syn v = Expr.of_value v
  let learn_eq (s : syn) (v : t) = Rustsymex.Consumer.learn_eq s v
  let exprs_syn v = [ v ]
  let subst = Expr.subst
  let pp = ppa
  let show (v : t) = Fmt.to_to_string pp v
  let sem_eq = sem_eq_untyped
  let fresh () = L.failwith "Fresh not implemented for opaque values"
end

(** The owned and dropped values of a type variable *)
module Block = struct
  type t = Value.t opaque [@@deriving show { with_path = false }]
  type syn = Value.syn opaque [@@deriving show { with_path = false }]

  let pp ft t = pp_opaque Value.pp ft t

  module SM =
    Soteria.Sym_states.State_monad.Make
      (Rustsymex)
      (struct
        type nonrec t = t option
      end)

  let owned_fix fix = ([ fix ], [])
  let dropped_fix fix = ([], [ fix ])

  let to_syn (owned, dropped) =
    [ (List.map Value.to_syn owned, List.map Value.to_syn dropped) ]

  let ins_outs ((owned, dropped) : syn) =
    (List.concat_map Value.exprs_syn (owned @ dropped), [])

  let of_opt (st : SM.st) : t = match st with None -> ([], []) | Some st -> st
  let to_opt (st : t) : SM.st = match st with [], [] -> None | _ -> Some st

  (* A zero-sized value read from memory has lost its identity: it stands for
     any value of its type variable. *)
  let is_wildcard v = Option.is_some (Typed.Adt.as_type_var_opt v)

  (* Symbolic process that removes one element equal to [v], if any *)
  let remove_sym (v : Value.t) (l : Value.t list) :
      Value.t list option Rustsymex.t =
    let open Rustsymex.Syntax in
    let rec aux l passed =
      match l with
      | [] -> Rustsymex.return None
      | x :: l when is_wildcard v || is_wildcard x ->
          Rustsymex.return (Some (List.rev_append passed l))
      | x :: l ->
          if%sat Value.sem_eq x v then
            Rustsymex.return (Some (List.rev_append passed l))
          else aux l (x :: passed)
    in
    aux l []

  (* Distinct values are distinct resources *)
  let distinct (owned, dropped) =
    Typed.distinct (List.filter (Fun.negate is_wildcard) (owned @ dropped))

  open SM.Syntax

  let drop (v : Value.t) : (unit, 'err, syn list) SM.Result.t =
    let* st = SM.get_state () in
    let owned, dropped = of_opt st in
    let*^ owned = remove_sym v owned in
    match owned with
    | None -> SM.Result.miss [ [ owned_fix (Value.to_syn v) ] ]
    | Some owned -> SM.Result.set_state (to_opt (owned, v :: dropped))

  let produce ((owned, dropped) : syn) (st : SM.st) : SM.st Rustsymex.Producer.t
      =
    let open Rustsymex.Producer.Syntax in
    let subst_all sf = List.map (Value.subst sf) in
    let* new_owned = Rustsymex.Producer.apply_subst subst_all owned in
    let* new_dropped = Rustsymex.Producer.apply_subst subst_all dropped in
    let owned, dropped = of_opt st in
    let st = (new_owned @ owned, new_dropped @ dropped) in
    let+^ () = Rustsymex.assume [ distinct st ] in
    to_opt st

  let consume ((owned, dropped) : syn) (st : SM.st) :
      (SM.st, syn list) Rustsymex.Consumer.t =
    let open Rustsymex.Consumer.Syntax in
    let rec consume_all ~lift_fix l = function
      | [] -> Rustsymex.Consumer.ok l
      | s :: rest -> (
          let* v = Rustsymex.Consumer.apply_subst Value.subst s in
          let*^ l = remove_sym v l in
          match l with
          | None -> Rustsymex.Consumer.miss [ [ lift_fix (Value.to_syn v) ] ]
          | Some l -> consume_all ~lift_fix l rest)
    in
    let st_owned, st_dropped = of_opt st in
    let* st_owned = consume_all ~lift_fix:owned_fix st_owned owned in
    let+ st_dropped = consume_all ~lift_fix:dropped_fix st_dropped dropped in
    to_opt (st_owned, st_dropped)
end

include
  Soteria.Sym_states.Pmap.Concrete
    (Rustsymex)
    (struct
      type t = Charon.Types.type_var_id

      let compare = Charon.Types.TypeVarId.compare_id
      let pp = Charon.Types.pp_type_var_id
      let show = Charon.Types.show_type_var_id
    end)
    (Block)

let drop ty_var v = wrap ty_var (Block.drop v)
