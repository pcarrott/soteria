open Charon
module Config_ = Config
open Soteria.Soteria_std
open Syntaxes.FunctionWrap
open Soteria_rust_lib
module Config = Config_
module Typed = Witness.Typed

module State = struct
  include State.Tree_state.Make (Tree_borrows.Concrete.Make)

  let rename_tvars f : syn -> syn =
    let mem_val : Tree_block.MemVal.syn -> Tree_block.MemVal.syn = function
      | SScalar v -> SScalar (Typed.rename_tvars f v)
      | SAggregate (v, ty) ->
          SAggregate (Typed.rename_tvars f v, Types.map_ty f ty)
      | v -> v
    in
    let block : Tree_block.syn -> Tree_block.syn = function
      | MemVal m -> MemVal { m with v = mem_val m.v }
      | Bound _ as b -> b
    in
    function
    | Ser_opaques (id, (owned, dropped)) ->
        Ser_opaques
          ( f id,
            ( List.map (Typed.rename_tvars f) owned,
              List.map (Typed.rename_tvars f) dropped ) )
    | Ser_pointers (Sptr.DecayMap.Ser_layouts (id, size)) ->
        Ser_pointers (Sptr.DecayMap.Ser_layouts (f id, size))
    | Ser_heap (loc, ({ node = Alive (Ser_block b); _ } as heap)) ->
        Ser_heap (loc, { heap with node = Alive (Ser_block (block b)) })
    | syn -> syn

  let opaque_syn id v = Ser_opaques (id, ([ v ], []))

  let owns_free_tvar (ty : Types.ty) (spatial : syn list) =
    List.exists
      (function
        | Ser_opaques (id, (owned, _)) ->
            not (List.is_empty owned || Types.mentions id ty)
        | Ser_heap _ | Ser_pointers _ -> false)
      spatial
end

module Logic = struct
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
            Typed.Svalue.Bool.mk_exists binders (Typed.Svalue.Bool.conj exprs)
        end)
  end
end

module Value = Witness.Value
module Prune = Prune.Make (State)

type t = {
  ret : Value.syn;
  asrt : State.syn Logic.Asrt.t;
  witness : Witness.syn;
}

let pp fmt = function
  | { ret; asrt; witness } ->
      Fmt.pf fmt "{\n ret = %a;\n asrt = %a;\n witness = %a\n}" Value.pp_syn ret
        (Logic.Asrt.pp State.pp_syn)
        asrt Witness.pp_syn witness

let nondet (ty : Types.ty) : t list =
  Rustsymex.run ~stats:Caller ~mode:UX @@ Value.nondet ty
  |> ListLabels.map ~f:(function ret, pure ->
      let ret = Value.to_syn ret in
      let asrt =
        let opaque id = State.opaque_syn id ret in
        Logic.Asrt.make ~pure
          ~spatial:(match ty with TVar (Free id) -> [ opaque id ] | _ -> [])
      in
      let witness = Witness.Value (ty, ret) in
      { ret; asrt; witness })

let make (ret : Value.t) (witness : Witness.t) (st : State.SM.st)
    (pcs : Typed.Expr.t list) ~(forgotten : Value.t list) ~(ty : Types.ty)
    ~(sized : Types.type_var_id list) : (t, [> `MemoryLeak ]) result =
  let open Result.Syntax in
  let ret = Value.to_syn ret in
  let spatial = State.to_syn @@ State.of_opt st in
  let forgotten = List.map Value.to_syn forgotten in
  let check_leaks = not (Config.get ()).ignore_leaks in
  let* spatial, pure =
    Prune.prune ~check_leaks ~sized ~ret ~forgotten ~pcs spatial
  in
  let+ () =
    (* Owned values of a type variable that [ty] does not mention are leaked, as
       the value of the summary cannot hold them. *)
    if check_leaks && State.owns_free_tvar ty spatial then
      Result.error `MemoryLeak
    else Result.ok ()
  in
  let asrt = Logic.Asrt.make ~spatial ~pure in
  let witness = Witness.to_syn witness in
  { ret; asrt; witness }

let produce (summ : t) (st : State.SM.st) :
    (Value.t * Witness.t * State.SM.st) Rustsymex.Producer.t =
  let open Rustsymex.Producer.Syntax in
  let* ret = Rustsymex.Producer.apply_subst Value.subst summ.ret in
  let* st = Logic.Asrt.Execute.produce summ.asrt st in
  let* witness = Rustsymex.Producer.apply_subst Witness.subst summ.witness in
  Rustsymex.Producer.return (ret, witness, st)

let consume (summ : t) (ret : Value.t) (st : State.SM.st) :
    (State.SM.st, State.syn list) Rustsymex.Consumer.t =
  let open Rustsymex.Consumer.Syntax in
  (* The sizes in a pointer depend on the layouts of the type variables: the
     atoms that bind them have to be consumed before learning from [ret]. *)
  let* remaining, st = Logic.Asrt.Execute.consume_partial summ.asrt st in
  let* () = Value.learn_eq summ.ret ret in
  Logic.Asrt.Execute.consume remaining st

let implies ?names s_pre s_post =
  let section_name =
    match names with
    | Some (pre, post) -> Fmt.str "Checking %s implies %s" pre post
    | None -> "Checking summary implication"
  in
  let@ () = L.with_section section_name in
  let process =
    let open Rustsymex.Syntax in
    (* Make sure we run the producer inside the symex *)
    let* () = Rustsymex.return () in
    let subst = Typed.Expr.Subst.empty in
    let* (ret, _, st), _ =
      Rustsymex.Producer.run ~subst @@ produce s_pre State.empty
    in
    Rustsymex.Consumer.run ~subst @@ consume s_post ret st
  in
  match Rustsymex.run ~stats:Caller ~mode:OX process with
  | [ (Soteria.Soteria_std.Compo_res.Ok (st, _), _) ] -> st = State.empty
  | _ -> false
  (* If the check is not supported, conservatively keep both summaries *)
  | exception Soteria.Symex.Gave_up _ -> false

(* Renames the type variables of [summ] with [f]. *)
let rename_tvars f (summ : t) : t =
  {
    ret = Value.rename_tvars f summ.ret;
    asrt = Logic.Asrt.rename_tvars State.rename_tvars f summ.asrt;
    witness = Witness.rename_tvars f summ.witness;
  }

module Context = struct
  module M = Stdlib.Map.Make (struct
    type t = Types.ty

    let compare = Types.compare_ty
  end)

  exception TypeNotSupported

  let type_not_supported (_ : Types.ty) = raise TypeNotSupported

  (* Each custom type has separate lists for visited, unvisited and staged
     summaries *)
  type value = Base of t list | Custom of t list * t list * t list
  type nonrec t = (t list * t list * t list) M.t

  let empty : t = M.empty

  let is_base_ty ty =
    match ty with Types.(TScalar _ | TVar _) -> true | _ -> false

  let ( let@ ) (ty, default) f =
    match ty with Types.TAdt _ -> f () | _ -> default

  let find ty (ctx : t) : value =
    match ty with
    | Types.TAdt _ ->
        let key = Types.canonical ty in
        let visited, unvisited, staged =
          Option.value ~default:([], [], []) (M.find_opt key ctx)
        in
        let inst = List.map (rename_tvars (Types.of_canonical ty)) in
        Custom (inst visited, inst unvisited, inst staged)
    | ty when is_base_ty ty -> Base (nondet ty)
    | ty -> type_not_supported ty

  let iter_summs tys (ctx : t) f =
    let rec aux ?(visited = false) ?(curr = []) ?(acc = []) = function
      | [] ->
          let acc = if visited then acc else curr :: acc in
          let rec aux' ?(acc = []) = function
            | [] -> f (List.rev acc)
            | summs :: rest ->
                List.iter (fun s -> aux' ~acc:(s :: acc) rest) summs
          in
          List.iter aux' acc
      | Base summs :: values -> aux values ~curr:(summs :: curr) ~acc
      | Custom (visited, unvisited, _) :: values ->
          let summs =
            ListLabels.fold_left values ~init:(unvisited :: curr)
              ~f:(fun acc -> function
              | Base summs -> summs :: acc
              | Custom (visited, unvisited, _) -> (visited @ unvisited) :: acc)
          in
          aux values ~visited:true ~curr:(visited :: curr) ~acc:(summs :: acc)
    in
    try aux (List.rev_map (fun ty -> find ty ctx) tys)
    with TypeNotSupported -> ()

  let stage ty summ (ctx : t) : t =
    let@ () = (ty, ctx) in
    let key = Types.canonical ty in
    let summ = rename_tvars (Types.to_canonical ty) summ in
    let exception SummaryAlreadyExists in
    let[@tail_mod_cons] rec filter i = function
      | [] -> []
      | x :: l ->
          if implies ~names:("this summary", Fmt.str "summary %d" i) summ x then
            raise SummaryAlreadyExists
            (* The new summary implies an existing summary: discard the new
               summary *)
          else if implies ~names:(Fmt.str "summary %d" i, "this summary") x summ
          then filter (i + 1) l
            (* An existing summary implies the new summary: discard the existing
               summary *)
          else x :: filter (i + 1) l
    in
    let opt_cons = function
      | None -> Some ([], [], [ summ ])
      | Some (visited, unvisited, staged) as summs -> (
          try
            Some (filter 0 visited, filter 0 unvisited, summ :: filter 0 staged)
          with SummaryAlreadyExists -> summs)
    in
    M.update key opt_cons ctx

  let commit (ctx : t) : t = M.map (fun (v, u, s) -> (u @ v, s, [])) ctx

  let pp_summs kind fmt = function
    | [] -> Fmt.pf fmt "%s: none" kind
    | summs ->
        Fmt.pf fmt "@[<v 2>%s:@ %a@]" kind (Fmt.list ~sep:Fmt.cut pp) summs

  let pp fmt (ctx : t) =
    let pp_entry fmt (key, (visited, unvisited, staged)) =
      Fmt.pf fmt "@[<v 2>%a:@ %a@ %a@ %a@]"
        (Crate.mk_pp PrintFmt.pp_ty)
        key (pp_summs "visited") visited (pp_summs "unvisited") unvisited
        (pp_summs "staged") staged
    in
    Fmt.pf fmt "@[<v>%a@]" (Fmt.list ~sep:Fmt.cut pp_entry) (M.bindings ctx)
end
