open Charon
open Soteria_rust_lib
module Typed = Svalue.Typed

module type State =
    module type of State.Tree_state.Make (Tree_borrows.Concrete.Make)

module Make (State : State) = struct
  (** {2 Reachability} *)

  (* The location variables of a block, and the variables it points to *)
  let iter_vars syn =
    let ins, outs = State.ins_outs syn in
    let iter_vars_l l =
      Iter.of_list l |> Iter.flat_map State.StateKey.Svalue.iter_vars
    in
    let is_loc (_, ty) = [%matches? Soteria.Bv_values.Svalue.TLoc _] ty in
    let ins = Iter.persistent_lazy (iter_vars_l ins) in
    ( Iter.filter is_loc ins,
      Iter.append (Iter.filter (Fun.negate is_loc) ins) (iter_vars_l outs) )

  (* Whether a location variable of a block satisfies [p] *)
  let reaches p syn =
    let locs, _ = iter_vars syn in
    Iter.exists (fun (x, _) -> p x) locs

  module Var_graph = struct
    include Soteria.Soteria_std.Graph.Make_in_place (Typed.Svalue.Var)

    (* For each equality [e1 = e2] in the path condition, we add a double edge
       from all variables of [e1] to all variables of [e2] *)
    let add_pc_edges graph pcs =
      ListLabels.iter pcs ~f:(fun v ->
          match Typed.Svalue.kind v with
          | Binop (Eq, el, er) ->
              (* We make the second iterator peristent to avoid going over the
                 structure too many times if there are many *)
              let r_iter = Iter.persistent_lazy @@ Typed.Svalue.iter_vars er in
              let product = Iter.product (Typed.Svalue.iter_vars el) r_iter in
              product (fun ((x, _), (y, _)) -> add_double_edge graph x y)
          | _ -> ())
  end

  module Var_hashset = Var_graph.Node_set

  (* For each block $l -> B in the post state, we add a single-sided arrow from
     the location variables in $l to all variables contained in B.

     For each decayed pointer $l -> i, we add a double edge from the location
     variables in $l to all variables in i. *)
  let add_block_edges graph spatial =
    ListLabels.iter spatial ~f:(fun syn ->
        let locs, outs = iter_vars syn in
        let add_edge =
          match syn with
          | State.Ser_heap _ | State.Ser_opaques _ -> Var_graph.add_edge graph
          | State.Ser_pointers _ -> Var_graph.add_double_edge graph
        in
        Iter.product locs (Iter.persistent_lazy outs) (fun ((x, _), (y, _)) ->
            add_edge x y))

  (* The reachability graph of variables, following path condition equalities
     and the contents of the blocks of the post state. *)
  let var_graph ~spatial ~pcs =
    let graph = Var_graph.with_node_capacity 0 in
    Var_graph.add_pc_edges graph pcs;
    add_block_edges graph spatial;
    graph

  (* The variables reachable in [graph] from [vars] *)
  let reachable_vars graph vars =
    let init_reachable = Var_hashset.with_capacity 0 in
    vars (fun (x, _) -> Var_hashset.add init_reachable x);
    Var_graph.reachable_from graph init_reachable

  (* The variables of the layouts of the type variables in the post state *)
  let iter_layout_vars spatial f =
    ListLabels.iter spatial ~f:(function
      | State.Ser_pointers (Sptr.DecayMap.Ser_layouts (_, size)) ->
          Typed.Svalue.iter_vars size f
      | _ -> ())

  (** {2 Leaks} *)

  type leaks = {
    sized : Types.type_var_id list;
        (** The type variables that may not be zero-sized *)
    reachable : Var_hashset.t;
        (** The variables reachable from the value of the summary *)
    forgotten : Var_hashset.t;
        (** The variables reachable from a forgotten value *)
  }

  (* An owned value of a type variable in [sized], which may not be zero-sized,
     keeps its identity in memory: it is leaked when none of its variables is
     reachable, from the value of the summary or from a forgotten value. *)
  let is_opaque_leak leaks owned =
    let reached (x, _) =
      Var_hashset.mem leaks.reachable x || Var_hashset.mem leaks.forgotten x
    in
    List.exists
      (fun v ->
        let vars = Typed.Svalue.iter_vars v in
        (not (Iter.is_empty vars)) && not (Iter.exists reached vars))
      owned

  (* A block of the post state that is unreachable but still alive on the heap
     is leaked, unless it is reachable from a forgotten value, whose drop may
     have freed it. Owned values of a type variable are leaked when they are
     unreachable. *)
  let is_leak leaks (syn : State.syn) =
    match syn with
    | State.Ser_opaques (id, (owned, _)) ->
        List.exists (Types.TypeVarId.equal_id id) leaks.sized
        && is_opaque_leak leaks owned
    | State.Ser_heap (_, block) ->
        State.Freeable_block_with_meta.is_leakable block
        && (not (reaches (Var_hashset.mem leaks.reachable) syn))
        && not (reaches (Var_hashset.mem leaks.forgotten) syn)
    | State.Ser_pointers _ -> false

  (** {2 Pruning} *)

  (* The values of the type variable [id] that are still reachable. An
     unreachable value can never be used again: a dropped one is forgotten, and
     an owned one of a zero-sized type variable, whose identity is lost, stands
     for any value of its type variable. *)
  let reachable_opaques leaks id (owned, dropped) =
    let reached v =
      Iter.exists
        (fun (x, _) ->
          Var_hashset.mem leaks.reachable x || Var_hashset.mem leaks.forgotten x)
        (Typed.Svalue.iter_vars v)
    in
    let zero_sized =
      not (List.exists (Types.TypeVarId.equal_id id) leaks.sized)
    in
    let owned =
      ListLabels.map owned ~f:(fun v ->
          if zero_sized && not (reached v) then
            Typed.Expr.of_value (Typed.Adt.mk_poly id)
          else v)
    in
    (owned, List.filter reached dropped)

  (* Keeps only the blocks of the post state whose location is reachable, and
     fails if any of them is leaked. *)
  let reachable_blocks_or_leaks ~check_leaks leaks spatial =
    Monad.ResultM.fold_list spatial ~init:[] ~f:(fun kept syn ->
        match syn with
        | _ when check_leaks && is_leak leaks syn -> Result.error `MemoryLeak
        | State.Ser_opaques (id, opaques) ->
            Result.ok
              (State.Ser_opaques (id, reachable_opaques leaks id opaques)
              :: kept)
        | Ser_pointers (Sptr.DecayMap.Ser_layouts _) -> Result.ok (syn :: kept)
        | _ when reaches (Var_hashset.mem leaks.reachable) syn ->
            Result.ok (syn :: kept)
        | _ -> Result.ok kept)
    |> Result.map List.rev

  (* The location of each block of the post state. *)
  let locs_of_spatial spatial =
    ListLabels.fold_left spatial ~init:[] ~f:(fun acc -> function
      | State.Ser_heap (loc, _) when not (List.mem loc acc) -> loc :: acc
      | State.Ser_heap _ | State.Ser_pointers _ | State.Ser_opaques _ -> acc)

  (* The location variable [sv], if it is one *)
  let as_loc_var sv =
    match Typed.Svalue.kind sv with
    | Var x
      when Iter.exists
             (fun (_, ty) -> [%matches? Soteria.Bv_values.Svalue.TLoc _] ty)
             (Typed.Svalue.iter_vars sv) ->
        Some x
    | _ -> None

  (* The values that [v] states to be pairwise distinct, if it is a [distinct]
     or a disequality *)
  let diseq_elems v =
    match Typed.Svalue.kind v with
    | Nop (Distinct, l) -> Some l
    | Unop (Not, e) -> (
        match Typed.Svalue.kind e with
        | Binop (Eq, l, r) -> Some [ l; r ]
        | _ -> None)
    | _ -> None

  (* The unreachable location variables of [pcs] that do not occur in [spatial]
     and only occur in [pcs] as elements of [distinct] atoms and disequalities.
     Such a variable can always be given an address distinct from all others, so
     removing it from these atoms eliminates its existential exactly. *)
  let droppable_locs ~reachable ~pcs ~spatial =
    let candidates = Var_hashset.with_capacity 0 in
    let blocked = Var_hashset.with_capacity 0 in
    let block sv =
      Typed.Svalue.iter_vars sv (fun (x, _) -> Var_hashset.add blocked x)
    in
    ListLabels.iter spatial ~f:(fun syn ->
        let locs, outs = iter_vars syn in
        Iter.append locs outs (fun (x, _) -> Var_hashset.add blocked x));
    ListLabels.iter pcs ~f:(fun v ->
        match diseq_elems v with
        | Some l ->
            ListLabels.iter l ~f:(fun sv ->
                match as_loc_var sv with
                | Some x when not (Var_hashset.mem reachable x) ->
                    Var_hashset.add candidates x
                | _ -> block sv)
        | None -> block v);
    fun sv ->
      match as_loc_var sv with
      | Some x ->
          Var_hashset.mem candidates x && not (Var_hashset.mem blocked x)
      | None -> false

  (* The path condition atoms that mention a reachable variable, where
     [distinct] atoms and disequalities are projected away from the droppable
     location variables, minus the [distinct] atoms that the single distinct
     assertion over [locs] subsumes. *)
  let relevant_pcs ~reachable ~spatial ~locs pcs =
    (* Ignore assertions with unreachable variables *)
    let pcs =
      ListLabels.filter pcs ~f:(fun v ->
          Iter.exists (fun (var, _) -> Var_hashset.mem reachable var)
          @@ Typed.Svalue.iter_vars v)
    in
    let droppable = droppable_locs ~reachable ~pcs ~spatial in
    ListLabels.filter_map pcs ~f:(fun v ->
        match Typed.Svalue.kind v with
        | Nop (Distinct, l) -> (
            let l' = List.filter (Fun.negate droppable) l in
            match l' with
            | [] | [ _ ] -> None
            (* Replace all distincts with a single distinct assertion *)
            | _ when List.for_all (fun sv -> List.mem sv locs) l' -> None
            | _ when List.compare_lengths l l' = 0 -> Some v
            | _ -> Some (Typed.Svalue.Bool.distinct l'))
        | Unop (Not, e) -> (
            match Typed.Svalue.kind e with
            | Binop (Eq, l, r) when droppable l || droppable r -> None
            | _ -> Some v)
        | _ -> Some v)

  (* The pure part of the summary: the relevant path condition atoms, preceded
     by a single assertion stating that all locations are distinct. *)
  let pure_of_pcs ~reachable ~spatial pcs =
    let locs = locs_of_spatial spatial in
    let filtered = relevant_pcs ~reachable ~spatial ~locs pcs in
    let distinct = Typed.Svalue.Bool.distinct locs in
    match Typed.Svalue.Bool.to_bool distinct with
    | Some true -> filtered
    | _ -> distinct :: filtered

  (* Prunes the post state [spatial] and the path condition [pcs] of a summary
     whose value is [ret], keeping only what is reachable from [ret]; if
     [check_leaks], fails if this drops a leaked value. *)
  let prune ~(check_leaks : bool) ~(sized : Types.type_var_id list)
      ~(ret : Typed.Expr.t) ~(forgotten : Typed.Expr.t list)
      ~(pcs : Typed.Expr.t list) (spatial : State.syn list) :
      (State.syn list * Typed.Expr.t list, [> `MemoryLeak ]) result =
    let open Result.Syntax in
    let from_graph = reachable_vars @@ var_graph ~spatial ~pcs in
    let reachable =
      from_graph Iter.(Typed.Svalue.iter_vars ret <+> iter_layout_vars spatial)
    in
    let forgotten =
      from_graph Iter.(of_list forgotten >>= Typed.Svalue.iter_vars)
    in
    let leaks = { sized; reachable; forgotten } in
    let+ spatial = reachable_blocks_or_leaks ~check_leaks leaks spatial in
    let pure = pure_of_pcs ~reachable ~spatial pcs in
    (spatial, pure)
end
