open Syntaxes.FunctionWrap
open Util
open Svalue
module Svalue = Typed.Svalue
module T = Typed.T
module Var = Soteria.Symex.Var
module Block = Bi_state.State.Freeable_block_with_meta

type after_exec = [ `After_exec ]
type pruned = [ `Pruned ]
type analysed = [ `Analysed ]

type raw = {
  args : Typed.Expr.t list;
      (** List of arguments values, corresponding to the formal arguments in
          order. Really a form of [(x == a0) * (y == a1)] *)
  pre : Bi_state.syn list;  (** Pre-condition as a list of fixes *)
  pc : Typed.Expr.t list; [@printer pp_pc]
      (** Path condition. Whether it is in the post or in the pre, it doesn't
          matter for UX. *)
  post : Bi_state.syn list;  (** Post condition as a serialized heap *)
  ret : (Typed.Expr.t, Error.with_trace) result;
      (** Return value. If `ok` then it is the C value that the function
          returned, if `err` then it is a description of the bug exhibitied by
          the code *)
}
[@@deriving show { with_path = false }]

let is_panic_from_helper error fn =
  let is_panic = match error with `Panic _, _ -> true | _ -> false in
  is_panic && Dataflow.Never.is_never fn

let leaks_to_error ~(fn : Charon.UllbcAst.fun_decl) =
  let name = fn.item_meta.name in
  let loc = fn.item_meta.span.data in
  function
  | [] ->
      (* There was a leak and we couldn't trace where it's from *)
      [
        Error.decorate
          (Trace.toplevel ~name ~loc
             ~op:"Leaking function (couldn't trace exact allocation)" ())
          `MemoryLeak;
      ]
  | l -> List.map (fun t -> Error.decorate t `MemoryLeak) l

type _ t =
  | After_exec : raw -> after_exec t
  | Pruned : { raw : raw; memory_leaks : Trace.t list option } -> pruned t
  | Analysed : {
      raw : raw;
      manifest_bugs : Error.with_trace list;
    }
      -> analysed t

let pp : type a. Format.formatter -> a t -> unit =
 fun ft summary ->
  match summary with
  | After_exec raw -> Fmt.pf ft "@[<2>After_exec@ %a@]" (Fmt.parens pp_raw) raw
  | Pruned { raw; _ } ->
      Fmt.pf ft "@[<v 2>Pruned {@ @[raw =@ %a@]}@]" pp_raw raw
  | Analysed { raw; manifest_bugs } ->
      Fmt.pf ft "@[<v 2>Analysed {@ @[raw =@ %a@];@ @[manifest_bugs =@ %a@]}@]"
        pp_raw raw
        (Fmt.Dump.list Error.pp_with_trace)
        manifest_bugs

module Var_graph = Graph.Make_in_place (Var)
module Var_hashset = Var_graph.Node_set

module Leak_set = Hashset.Make (struct
  type t = Trace.t option

  let equal = ( = )
  let hash = Hashtbl.hash
  let pp = Fmt.Dump.option Trace.pp
end)

let is_relevant relevant_vars v =
  Iter.exists
    (fun (var, _) -> Var_hashset.mem relevant_vars var)
    (Svalue.iter_vars v)

let filter_pc relevant_vars = List.filter @@ is_relevant relevant_vars

(** Removes any bit of the state that does not have any "relevant variables".
    i.e., bits that are not reachable from the precondition. *)
let filter_serialized_state relevant_vars (state : Bi_state.syn list) =
  let is_relevant = is_relevant relevant_vars in
  let leak_origins = Leak_set.with_capacity 0 in
  let filtered =
    ListLabels.filter state ~f:(function
      | Bi_state.State.Ser_heap (loc, b) ->
          let keep = is_relevant loc in
          if not keep then
            [%l.trace "Removing dead location %a" Typed.Expr.pp loc];
          if (not keep) && Block.is_leakable b then (
            [%l.trace "Location %a leaked" Typed.Expr.pp loc];
            Leak_set.add leak_origins (Block.trace b));
          keep
      | Ser_pointers (Sptr.DecayMap.Ser_decays (loc, addr)) ->
          (* TODO: Should we signal as a warning any address that is kept but
             not exposed? *)
          is_relevant loc || is_relevant addr.address
      | Ser_pointers (Sptr.DecayMap.Ser_layouts _) -> true)
  in
  (filtered, Leak_set.to_seq leak_origins |> List.of_seq)

let init_reachable_vars summary =
  let init_reachable = Var_hashset.with_capacity 0 in
  let mark_reachable x = Var_hashset.add init_reachable x in
  let mark_cval_reachable cval =
    (cval |> Typed.Svalue.iter_vars) @@ fun (x, _) -> mark_reachable x
  in
  (* We mark all variables from the arguments and return value as reachable *)
  let () = List.iter mark_cval_reachable summary.args in
  let () = Result.iter mark_cval_reachable summary.ret in
  (* TODO: mark all globals as reachable. *)
  init_reachable

let make ~args ~pre ~pc ~post ~ret () = After_exec { args; pre; pc; post; ret }

module Logic = Soteria.Logic.Make (Rustsymex)
module Execute = Logic.Asrt.Execute (Bi_state.State)

(* TODO: for below: do we only need to compute reachability of variables of type
   Loc, since they are the only one that cannot be created out of the blue, and
   need provenance? *)

(** This function prunes the summary by removing anything that isn't reachable
    from the arguments or the return value. It returns the updated summary, as
    well as a boolean capturing whether a memory leak was detected. A memory
    leak is detected if there was an unreachable block that was not freed. *)
let prune ~name (summary : after_exec t) : pruned t =
  let (After_exec summary) = summary in
  let@ () =
    L.with_section (Fmt.str "Pruning a summary for %a" Crate.pp_name name)
  in
  [%l.trace "Pruning summary %a" pp_raw summary];
  let module Var_graph = Graph.Make_in_place (Var) in
  let graph = Var_graph.with_node_capacity 0 in
  let init_reachable = init_reachable_vars summary in
  (* For each equality [e1 = e2] in the path condition, we add a double edge
     from all variables of [e1] to all variables of [e2] *)
  ListLabels.iter summary.pc ~f:(fun v ->
      match Svalue.kind v with
      | Binop (Eq, el, er) ->
          (* We make the second iterator peristent to avoid going over the
             structure too many times if there are many *)
          let r_iter = Iter.persistent_lazy (Svalue.iter_vars er) in
          let product = Iter.product (Svalue.iter_vars el) r_iter in
          product (fun ((x, _), (y, _)) -> Var_graph.add_double_edge graph x y)
      | _ -> ());
  (* For each block $l -> B in the pre and post state, we add a single-sided
     arrow from all variables in $l to all variables contained in B.

     For each *exposed* decayed pointer $l -> i, we add a double edge from all
     variables in $l to all variables in i. *)
  let all_fragments = summary.pre @ summary.post in
  ListLabels.iter all_fragments ~f:(function
    | Bi_state.State.Ser_pointers (Sptr.DecayMap.Ser_decays (l, addr)) ->
        let addr_iter = Iter.persistent_lazy (Svalue.iter_vars addr.address) in
        Iter.product (Svalue.iter_vars l) addr_iter (fun ((x, _), (y, _)) ->
            Var_graph.add_double_edge graph x y)
    (* The layouts of the type variables are always kept *)
    | Bi_state.State.Ser_pointers (Sptr.DecayMap.Ser_layouts (_, size)) ->
        Svalue.iter_vars size (fun (x, _) -> Var_hashset.add init_reachable x)
    | Bi_state.State.Ser_heap (l, b) ->
        let b_iter =
          let _, outs = Bi_state.State.Freeable_block_with_meta.ins_outs b in
          Iter.of_list outs |> Iter.flat_map Svalue.iter_vars
        in
        Iter.product (Svalue.iter_vars l) (Iter.persistent_lazy b_iter)
          (fun ((x, _), (y, _)) -> Var_graph.add_edge graph x y));
  (* [init_reachable] is the set of initially-reachable variables, and we have a
     reachability [graph]. We can compute all reachable values. *)
  let reachable = Var_graph.reachable_from graph init_reachable in
  (* We can now filter the summary to keep only the reachable values *)
  let new_pc = filter_pc reachable summary.pc in
  let new_post, memory_leaks = filter_serialized_state reachable summary.post in
  let memory_leaks =
    (* Memory leaks only make sense for functions that terminate successfully *)
    match (summary.ret, memory_leaks) with
    | Ok _, (_ :: _ as l) -> Some (List.filter_map Fun.id l)
    | _ -> None
  in
  Pruned { raw = { summary with pc = new_pc; post = new_post }; memory_leaks }

(** Current criterion: a bug is manifest if its path condition is a consequence
    of the heap's and function arguments well-formedness conditions *)
let rec analyse : type a. fn:Charon.UllbcAst.fun_decl -> a t -> analysed t =
 fun ~fn summary ->
  let name = fn.item_meta.name in
  match summary with
  | After_exec _ -> analyse ~fn (prune ~name summary)
  | Analysed _ -> summary
  | Pruned { raw = summary; memory_leaks } -> (
      let@ () =
        Fmt.kstr
          (L.with_section ~is_branch:false)
          "Analysing a summary for %a" Crate.pp_name name
      in
      [%l.info
        "Analysing a summary for %a@\n%a@\nwith leak set: %a" Crate.pp_name name
          pp_raw summary
          Fmt.Dump.(option @@ list Trace.pp)
          memory_leaks];
      match summary.ret with
      | Ok _ ->
          let manifest_leak =
            Option.fold ~none:[] ~some:(leaks_to_error ~fn) memory_leaks
          in
          Analysed { raw = summary; manifest_bugs = manifest_leak }
      | Error error ->
          (* This code could be slightly better if we make syntactic constraints
             on the arguments as the "pure" thing here, instead of building it
             separately *)
          let post_asrt = Logic.Asrt.make ~spatial:summary.post ~pure:[] in
          let process =
            let open Rustsymex.Syntax in
            let producer =
              let open Rustsymex.Producer in
              let open Syntax in
              let* substituted_args =
                map_list summary.args ~f:(fun syn ->
                    apply_subst Typed.Expr.subst syn)
              in
              let*^ () =
                List.combine fn.signature.inputs substituted_args
                |> Rustsymex.iter_list ~f:(fun (ty, arg) ->
                    let open Rustsymex in
                    let open Syntax in
                    let* res, _ =
                      Layout.SM.run_with_state ~state:None
                      @@ Value_codec.validity ty arg (fun constr _err ->
                          let open Layout.SM.Syntax in
                          let* () = Layout.SM.assume [ constr ] in
                          Layout.SM.Result.ok ())
                    in
                    match res with
                    | Ok () -> return ()
                    | Error _ | Missing _ -> vanish ())
              in
              (* We don't need the produced heap, just its wf condition *)
              (* We might want to use another symex monad, à grisette, that
               produces the condition as a writer monad in an \/ or
               something *)

              let* _ = Execute.produce post_asrt Bi_state.empty in
              (* At the end, we need to apply the current substitution to subst
                 the PC as well! *)
              fold_list summary.pc ~init:[] ~f:(fun acc sv ->
                  let+ v = apply_subst Typed.Expr.subst sv in
                  v :: acc)
            in
            let* pc, _ =
              Rustsymex.Producer.run ~subst:Typed.Expr.Subst.empty producer
            in
            [%l.trace
              "Produced heap, about to check if path condition holds in every \
               branch"];
            Rustsymex.assert_ (Typed.conj pc)
          in
          let is_manifest () =
            try
              let result = Rustsymex.run ~mode:OX process in
              (* The bug is manifest if the test passed in every branch. *)
              (not (List.is_empty result)) && List.for_all fst result
            with Soteria.Symex.Gave_up _ -> false
          in
          let is_manifest =
            (not (is_panic_from_helper error fn.def_id)) && is_manifest ()
          in
          if is_manifest then [%l.debug "Bug is manifest!!"];
          let manifest_bugs = if is_manifest then [ error ] else [] in
          Analysed { raw = summary; manifest_bugs })

let manifest_bugs (type a) ~fn (summary : a t) =
  let (Analysed { manifest_bugs; _ }) = analyse ~fn summary in
  manifest_bugs
