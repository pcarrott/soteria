open Soteria.Soteria_std
open Syntaxes.FunctionWrap
module Config_ = Config
open Soteria_rust_lib
module Config = Config_
module Typed = Svalue.Typed

module State = struct
  include State.Tree_state.Make (Tree_borrows.Concrete.Make)
end

module Logic = struct
  include Soteria.Logic.Make (Rustsymex)

  module Asrt = struct
    include Asrt

    module Execute =
      Execute_partial
        (State)
        (struct
          let mk_exists binders body =
            let binders =
              List.map (fun (var, ty) -> (var, Typed.untype_type ty)) binders
            in
            Typed.Svalue.Bool.mk_exists binders body

          let conj = Typed.Svalue.Bool.conj
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

let nondet (ty : Charon.Types.ty) : t list =
  Rustsymex.run ~stats:Caller ~mode:UX @@ Value.nondet ty
  |> ListLabels.map ~f:(function ret, pure ->
      let ret = Value.to_syn ret in
      let asrt = Logic.Asrt.make ~spatial:[] ~pure in
      let witness = Witness.Value (ty, ret) in
      { ret; asrt; witness })

let make (ret : Value.t) (witness : Witness.t) (st : State.SM.st)
    ~(forgotten : Value.t list) (pcs : Typed.Expr.t list) :
    (t, [> `MemoryLeak ]) result =
  let open Result.Syntax in
  let ret = Value.to_syn ret in
  let witness = Witness.to_syn witness in
  let spatial = State.to_syn @@ State.of_opt st in
  let forgotten = List.map Value.to_syn forgotten in
  let check_leaks = not (Config.get ()).ignore_leaks in
  let+ spatial, pure = Prune.prune ~check_leaks ~ret ~forgotten ~pcs spatial in
  let asrt = Logic.Asrt.make ~spatial ~pure in
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
  let* () = Value.learn_eq summ.ret ret in
  Logic.Asrt.Execute.consume summ.asrt st

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

module Context = struct
  open Charon.Types
  module M = TypeDeclId.Map

  exception TypeNotSupported

  let type_not_supported (_ : ty) = raise TypeNotSupported

  (* Each custom type has separate lists for visited, unvisited and staged
     summaries *)
  type value = Base of t list | Custom of t list * t list * t list
  type nonrec t = (t list * t list * t list) M.t

  let empty : t = M.empty
  let is_base_ty ty = match ty with TScalar _ -> true | _ -> false

  let ( let@ ) (ty, default) f =
    match ty with TAdt { id; _ } -> f id | _ -> default

  let find ty (ctx : t) : value =
    match ty with
    | TAdt { id; _ } ->
        let visited, unvisited, staged =
          Option.value ~default:([], [], []) (M.find_opt id ctx)
        in
        Custom (visited, unvisited, staged)
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
    let@ id = (ty, ctx) in
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
    M.update id opt_cons ctx

  let commit (ctx : t) : t = M.map (fun (v, u, s) -> (u @ v, s, [])) ctx

  let pp_summs kind fmt = function
    | [] -> Fmt.pf fmt "%s: none" kind
    | summs ->
        Fmt.pf fmt "@[<v 2>%s:@ %a@]" kind (Fmt.list ~sep:Fmt.cut pp) summs

  let pp fmt (ctx : t) =
    let pp_entry fmt (id, (visited, unvisited, staged)) =
      Fmt.pf fmt "@[<v 2>%a:@ %a@ %a@ %a@]" Crate.pp_name
        (Crate.get_adt_raw id).item_meta.name (pp_summs "visited") visited
        (pp_summs "unvisited") unvisited (pp_summs "staged") staged
    in
    Fmt.pf fmt "@[<v>%a@]" (Fmt.list ~sep:Fmt.cut pp_entry) (M.bindings ctx)
end
