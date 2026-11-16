open Charon
open Soteria_rust_lib
module Typed = Witness.Typed

(* An instance [∃x⃗. x⃗ = c⃗ ∧ P] of an existential [∃x⃗. P] implies it, so
   disjoining instances with the existential keeps it equivalent, while letting
   the solver refute its negation by eliminating the equations, without
   instantiating the quantifier itself. Instances are stated with equations
   rather than by substitution, as substituting an enum into a projection of
   another of its variants is rejected by the smart constructors. The candidate
   terms for a bound variable are the terms that a conjunct of [P] equates it
   to, through the fields of tuples and enums, then the free variables of [P] of
   its type. *)
module Instances = struct
  module Ext = Svalue.Ext
  module Svalue = Typed.Svalue

  let max_instances = 8

  (* The terms that the conjuncts [exprs] equate to the variables satisfying
     [is_bound] *)
  let matched_terms is_bound exprs =
    let mentions_bound v =
      Iter.exists (fun (x, _) -> is_bound x) (Svalue.iter_vars v)
    in
    let rec eq acc l r =
      match (Svalue.kind l, Svalue.kind r) with
      | Extension (Ext.Tuple ls), Extension (Ext.Tuple rs)
        when List.compare_lengths ls rs = 0 ->
          List.fold_left2 eq acc ls rs
      | Extension (Ext.Enum (var, ls)), Extension (Ext.Enum (var', rs))
        when Types.equal_variant_id var var' && List.compare_lengths ls rs = 0
        ->
          List.fold_left2 eq acc ls rs
      | Var x, _ when is_bound x && not (mentions_bound r) -> (x, r) :: acc
      | _, Var x when is_bound x && not (mentions_bound l) -> (x, l) :: acc
      | _ -> acc
    in
    Iter.of_list exprs
    |> Iter.flat_map Svalue.Bool.split_ands
    |> Iter.fold
         (fun acc v ->
           match Svalue.kind v with Binop (Eq, l, r) -> eq acc l r | _ -> acc)
         []

  (* The candidate terms for each of [binders] in [exprs] *)
  let candidates binders exprs =
    let is_bound x = List.exists (fun (y, _) -> Svalue.Var.equal x y) binders in
    let matched = matched_terms is_bound exprs in
    let free =
      Iter.of_list exprs
      |> Iter.flat_map Svalue.iter_vars
      |> Iter.filter (fun (x, _) -> not (is_bound x))
      |> Iter.sort_uniq ~cmp:(fun (x, _) (y, _) -> Svalue.Var.compare x y)
      |> Iter.to_list
    in
    ListLabels.map binders ~f:(fun (x, ty) ->
        let matched =
          List.filter_map
            (fun (y, t) -> if Svalue.Var.equal x y then Some t else None)
            matched
        in
        let vars =
          List.filter_map
            (fun (y, ty') ->
              if Svalue.equal_ty ty ty' then Some (Svalue.mk_var y ty) else None)
            free
        in
        (Svalue.mk_var x ty, List.sort_uniq Svalue.compare matched @ vars))

  (* The assignments of candidate terms to all the bound variables *)
  let assignments candidates =
    List.fold_right
      (fun (x, cs) tails ->
        Seq.flat_map
          (fun c -> Seq.map (fun tail -> (x, c) :: tail) tails)
          (List.to_seq cs))
      candidates (Seq.return [])

  let instance binders exprs assignment =
    let eqs = List.map (fun (x, c) -> Svalue.Bool.sem_eq x c) assignment in
    Svalue.Bool.mk_exists binders (Svalue.Bool.conj (eqs @ exprs))

  (* The values that [v] states to be pairwise distinct, if it is a [distinct]
     or a disequality *)
  let diseq_elems v =
    match Svalue.kind v with
    | Nop (Distinct, l) -> Some l
    | Unop (Not, e) -> (
        match Svalue.kind e with Binop (Eq, l, r) -> Some [ l; r ] | _ -> None)
    | _ -> None

  (* Eliminates the bound variables that only occur in the conjuncts [exprs] as
     elements of [distinct] atoms and disequalities, and whose type has more
     values than they have disequalities: they can always be given a value
     distinct from all others, so removing them from these atoms is exact. *)
  let eliminate_diseqs binders exprs =
    let conjuncts =
      Iter.of_list exprs |> Iter.flat_map Svalue.Bool.split_ands |> Iter.to_list
    in
    let is_var x v =
      match Svalue.kind v with Var y -> Svalue.Var.equal x y | _ -> false
    in
    let mentions x v =
      Iter.exists (fun (y, _) -> Svalue.Var.equal x y) (Svalue.iter_vars v)
    in
    (* The number of disequalities of [x], if it only occurs in such atoms *)
    let num_diseqs x =
      ListLabels.fold_left conjuncts ~init:(Some 0) ~f:(fun acc c ->
          let open Syntaxes.Option in
          let* n = acc in
          match diseq_elems c with
          | Some l when List.exists (is_var x) l ->
              if List.exists (fun e -> mentions x e && not (is_var x e)) l then
                None
              else Some (n + List.length l - 1)
          | _ -> if mentions x c then None else Some n)
    in
    let eliminable (x, ty) =
      let bits =
        match ty with
        | Ext.Sv.TExtension Ext.TPolyType ->
            Soteria_rust_lib.Svalue.Encoding.poly_size
        | TBitVector n | TLoc n -> n
        | _ -> 0
      in
      match num_diseqs x with
      | Some n ->
          let width = min bits 62 in
          bits >= 8 && Z.(lt (of_int n) (shift_left one width))
      | None -> false
    in
    let eliminated, binders = List.partition eliminable binders in
    let is_eliminated v = List.exists (fun (x, _) -> is_var x v) eliminated in
    let conjuncts =
      ListLabels.filter_map conjuncts ~f:(fun c ->
          match Svalue.kind c with
          | Nop (Distinct, l) -> (
              match List.filter (Fun.negate is_eliminated) l with
              | [] | [ _ ] -> None
              | l' when List.compare_lengths l l' = 0 -> Some c
              | l' -> Some (Svalue.Bool.distinct l'))
          | _ -> (
              match diseq_elems c with
              | Some l when List.exists is_eliminated l -> None
              | _ -> Some c))
    in
    (binders, conjuncts)

  let mk_exists binders exprs =
    let binders, exprs = eliminate_diseqs binders exprs in
    let exists = Svalue.Bool.mk_exists binders (Svalue.Bool.conj exprs) in
    match Svalue.kind exists with
    | Exists (binders, _) ->
        assignments @@ candidates binders exprs
        |> Seq.take max_instances
        |> Seq.map (instance binders exprs)
        |> List.of_seq
        |> List.fold_left (Fun.flip Svalue.Bool.or_) exists
    | _ -> exists
end

(* The logic of the summaries over the state [State] *)
module Make (State : Soteria.Sym_states.Base.M(Rustsymex).S) = struct
  include Soteria.Logic.Make (Rustsymex)

  module Asrt = struct
    include Asrt

    let rename_tvars f_syn f asrt =
      ListLabels.map asrt ~f:(function
        | Spatial syn -> Spatial (f_syn f syn)
        | Pure v -> Pure (Typed.rename_tvars f v))

    module Execute =
      Execute_partial
        (State)
        (struct
          let mk_exists binders exprs =
            let binders =
              List.map (fun (var, ty) -> (var, Typed.untype_type ty)) binders
            in
            Instances.mk_exists binders exprs
        end)
  end
end
