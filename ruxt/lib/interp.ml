open Charon
open Soteria_rust_lib
module State = Summary.State
module Typed = Summary.Typed
module Rust_interp = Interp.Make (State)

(* [State.SM] over the witness monad: the heap state is run before the witness
   is, and the [^] operators of its syntax lift computations of [State.SM]. The
   heap state is paired with the values that were forgotten rather than dropped,
   as their drop could not be run. *)
module StateM = struct
  module Base =
    Soteria.Sym_states.State_monad.Make
      (Witness.WM)
      (struct
        type t = State.SM.st * Summary.Value.t list
      end)

  include Base

  let with_call fun_decl = lift (Witness.WM.with_call fun_decl)
  let ret_var = lift Witness.WM.ret_var
  let add_arg arg = lift (Witness.WM.add_arg arg)
  let add_stmt stmt = lift (Witness.WM.add_stmt stmt)
  let set_result var = lift (Witness.WM.set_result var)
  let get_heap () = map (fun (st, _) -> st) (get_state ())
  let set_heap st = map_state (fun (_, forgotten) -> (st, forgotten))
  let forget v = map_state (fun (st, forgotten) -> (st, v :: forgotten))

  let lift_state (x : 'a State.SM.t) : 'a t =
   fun (st, forgotten) ->
    Witness.WM.lift
    @@ Rustsymex.map (fun (v, st) -> (v, (st, forgotten)))
    @@ x st

  let lift x = lift (Witness.WM.lift x)

  module Result = struct
    include Result

    (* Runs [x] from an empty heap and witness *)
    let run x =
      run_with_state ~state:(State.empty, []) x
      |> Witness.WM.run_with_witness ~witness:Witness.empty
  end

  module Syntax = struct
    include Syntax

    let ( let**^ ) x f = Result.bind f (lift_state x)
    let ( let++^ ) x f = Result.map f (lift_state x)
  end

  open Syntax

  (* Zero-sized values are not allocated: their pointers are dangling *)
  let free ptr =
    let ptr = Summary.Value.as_ptr ptr in
    match
      Typed.Svalue.Bool.to_bool
        (Typed.Expr.of_value (Typed.Ptr.has_provenance (Typed.Ptr.ptr_of ptr)))
    with
    | Some false -> Result.ok ()
    | _ -> lift_state @@ State.free ptr

  (* Allocates a pointer and stores [v] inside *)
  let box ty v =
    let**^ ptr = State.alloc_ty ty in
    let++^ () = State.store (Summary.Value.as_ptr ptr) ty v in
    ptr

  (* Reads the value behind [ptr] and frees it *)
  let unbox ty ptr =
    let**^ v = State.load (Summary.Value.as_ptr ptr) ty in
    let++ () = free ptr in
    v

  (* Runs an interpreter computation from the current state, dropping the state
     the interpreter pairs with its errors *)
  let lift_interp f =
    let* state = get_heap () in
    let** ret, state =
      lift @@ Rustsymex.map (Compo_res.map_error fst) @@ f ~state
    in
    let+ () = set_heap state in
    Compo_res.ok ret
end

open StateM.Syntax

let exec_fun ~args fun_decl =
  StateM.lift_interp (Rust_interp.exec_fun_compo ~args fun_decl)

(* Produces the summary [summ], returning its value along with its witness *)
let run_producer (summ : Summary.t) : (Summary.Value.t * Witness.t) StateM.t =
  let* state = StateM.get_heap () in
  let* (ret, witness, state), _ =
    let subst = Summary.Typed.Expr.Subst.empty in
    StateM.lift @@ Rustsymex.Producer.run ~subst @@ Summary.produce summ state
  in
  let+ () = StateM.set_heap state in
  (ret, witness)

module Drops = Drop.Make (struct
  include StateM.Base

  let lift = StateM.lift
  let lift_state = StateM.lift_state
  let forget = StateM.forget
  let box = StateM.box
  let free = StateM.free

  let exec_fn_ptr fn_ptr args =
    StateM.lift_interp (Rust_interp.exec_fn_ptr_compo ~args fn_ptr)
end)

(* Whether [ty] is [Copy], erring on the side of [false]: an ADT is only [Copy]
   with an impl of [Copy] for exactly this type *)
let rec is_copy (ty : Types.ty) =
  match ty with
  | TScalar _ | TRawPtr _ | TRef (_, _, RShared) | TFnPtr _ -> true
  | TAdt { builtin = Some TTuple; generics = { types; _ }; _ } ->
      List.for_all is_copy types
  | TAdt { builtin = None; _ } ->
      UllbcAst.TraitImplId.Map.exists
        (fun _ (impl : UllbcAst.trait_impl) ->
          (not impl.is_negative)
          && (Crate.get_trait_decl_raw impl.impl_trait.id).item_meta.lang_item
             = Some RustcLangItemCopy
          &&
          match impl.impl_trait.generics.types with
          | [ self ] -> Types.equal_ty self ty
          | _ -> false)
        (Crate.get_crate ()).trait_impls
  | TArray (ty, _, _) -> is_copy ty
  | _ -> false

(* [exec_write drops ty ptr v src] writes [v], witnessed by [src], behind the
   mutable reference output by the call with [std::mem::replace], and drops the
   old value, or forgets it if its drop cannot be run *)
let exec_write drops ty ptr v src =
  let* var = StateM.ret_var in
  let needs = Drop.needs_drop drops ty in
  let* old_var =
    match needs with
    | Drop.Unknown ->
        let+ keep = StateM.lift (Witness.fresh_name ()) in
        Some keep
    | Drop.Yes | Drop.No -> StateM.return None
  in
  let* () = StateM.add_stmt (Witness.Write (var, src, old_var)) in
  let**^ old = State.load (Summary.Value.as_ptr ptr) ty in
  let**^ () = State.store (Summary.Value.as_ptr ptr) ty v in
  match needs with
  | Drop.Unknown ->
      let* () = StateM.forget old in
      StateM.Result.ok ()
  | Drop.Yes | Drop.No -> Drops.drop_value drops ty old

(* [exec_read ty ptr] reads through the shared reference output by the call with
   safe code: by copying the value when it is [Copy], and otherwise by passing
   the reference to [std::hint::black_box], which validates and retags it on
   entry, like any reference argument, but does not read the value itself *)
let exec_read ty ptr =
  let* var = StateM.ret_var in
  let ptr = Summary.Value.as_ptr ptr in
  if is_copy ty then
    let* copy = StateM.lift (Witness.fresh_name ()) in
    let* () = StateM.add_stmt (Witness.Read (var, Some copy)) in
    let++^ _ = State.load ptr ty in
    ()
  else
    let* () = StateM.add_stmt (Witness.Read (var, None)) in
    let**^ () = State.Sptr.check_aligned ptr ty in
    let**^ () = State.Sptr.check_non_dangling ptr ty in
    let**^ ptr = State.borrow ~protect:true ptr (TRef (RErased, ty, RShared)) in
    StateM.lift_state @@ State.unprotect ptr ty

(* [exec_drop drops ty ?var value] drops the value named [var] in the witness,
   or the output of the call by default, which is either a [`Value], or in a
   [`Place] that it is moved out of. A value whose drop cannot be run is
   forgotten: the witness moves it to a fresh variable, which is only dropped at
   the end of the test, after all the statements that were analysed. *)
let exec_drop drops ty ?var value =
  let* var =
    match var with Some var -> StateM.return var | None -> StateM.ret_var
  in
  let needs = Drop.needs_drop drops ty in
  let* () =
    match (needs, value) with
    | Drop.Unknown, _ ->
        let* keep = StateM.lift (Witness.fresh_name ()) in
        StateM.add_stmt (Witness.Keep (keep, var))
    | Drop.Yes, _ | Drop.No, `Place _ -> StateM.add_stmt (Witness.Drop var)
    | Drop.No, `Value _ -> StateM.return ()
  in
  let** v =
    match value with
    | `Value v -> StateM.Result.ok v
    | `Place ptr -> StateM.unbox ty ptr
  in
  match needs with
  | Drop.Unknown ->
      let* () = StateM.forget v in
      StateM.Result.ok ()
  | Drop.Yes | Drop.No -> Drops.drop_value drops ty v
