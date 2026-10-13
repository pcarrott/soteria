open Soteria.Soteria_std
open Soteria_rust_lib
module State = Summary.State
module Interp = Interp.Make (State)
open State.SM.Syntax
open Charon

(* Runs an interpreter computation from the current state, dropping the state
   the interpreter pairs with its errors *)
let lift_interp f =
  let* state = State.SM.get_state () in
  let** ret, state =
    State.SM.lift @@ Rustsymex.map (Compo_res.map_error fst) @@ f ~state
  in
  let+ () = State.SM.set_state state in
  Compo_res.ok ret

let exec_fun ~args fun_decl = lift_interp (Interp.exec_fun_compo ~args fun_decl)

(* Executes [<ty as Destruct>::drop_glue(ptr)] like a drop desugared by Charon,
   where [drop_glue] is the drop glue of the ADT [ty]: its impl is instantiated
   with the generics of [ty], which carry the [Destruct] impls of the type
   parameters. *)
let exec_drop_glue (drop_glue : UllbcAst.fun_decl) (ty : Types.ty) ptr =
  match (drop_glue.src, ty) with
  | TraitImplFun (impl_ref, trait_decl_ref, method_id, _), TAdt { generics; _ }
    ->
      let trait_decl_ref =
        {
          trait_decl_ref with
          generics = { trait_decl_ref.generics with types = [ ty ] };
        }
      in
      let trait_ref : Types.trait_ref =
        {
          kind = TraitImpl { impl_ref with generics };
          trait_decl_ref =
            { binder_regions = []; binder_value = trait_decl_ref };
        }
      in
      let fn_ptr : Types.fn_ptr =
        {
          kind = TraitMethod (trait_ref, method_id);
          generics =
            { TypesUtils.empty_generic_args with regions = [ RErased ] };
        }
      in
      lift_interp (Interp.exec_fn_ptr_compo ~args:[ ptr ] fn_ptr)
  | _ -> failwith "Expected the drop glue of an ADT"

let alloc ty v =
  let** ptr = State.alloc_ty ty in
  let++ () = State.store (Summary.Value.as_ptr ptr) ty v in
  ptr

let exec_drop drops ty ~none ~some =
  match ty with
  | Types.TAdt { id; _ } -> (
      match Types.TypeDeclId.Map.find_opt id drops with
      | Some fun_decl ->
          let** ptr = some in
          let** _ = exec_drop_glue fun_decl ty ptr in
          State.free (Summary.Value.as_ptr ptr)
      | None -> none)
  | _ -> none

type fn =
  Summary.t list ->
  ( Types.ty * Summary.Value.t,
    Error.with_trace,
    State.syn list )
  State.SM.Result.t

type t = { name : Types.name; fn : fn }

let name { name; _ } = name

let call (fun_decl : UllbcAst.fun_decl) summs =
  let ty = fun_decl.signature.output in
  let if_rmut ~then_ ~else_ =
    let rmut_ty = match ty with TRef (_, ty, RMut) -> Some ty | _ -> None in
    Option.fold rmut_ty ~none:else_ ~some:then_
  in
  let summ = if_rmut ~then_:(fun _ -> Some (List.hd summs)) ~else_:None in
  let summs = if_rmut ~then_:(fun _ -> List.tl summs) ~else_:summs in
  (* Check reference arguments and allocate values on heap *)
  let** args, arg_ptrs =
    ListLabels.fold_left2 summs fun_decl.signature.inputs
      ~init:(State.SM.Result.ok ([], []))
      ~f:(fun acc summ ty ->
        let** args, arg_ptrs = acc in
        let* arg, _ =
          Summary.run_producer Summary.Typed.Expr.Subst.empty summ
        in
        match ty with
        | Types.TRef (_, ty, _) ->
            let++ ptr = alloc ty arg in
            (ptr :: args, (ty, ptr) :: arg_ptrs)
        | _ -> State.SM.Result.ok (arg :: args, arg_ptrs))
  in
  let args = List.rev args in
  (* Symbolically execute the function call *)
  let** ret = exec_fun fun_decl ~args in
  (* Handle the return value if it is a reference *)
  let++ () =
    match ty with
    | TRef (_, ty, RShared) ->
        (* For shared references, we simply read from the pointer *)
        let++ _ = State.load (Summary.Value.as_ptr ret) ty in
        ()
    | TRef (_, ty, RMut) ->
        (* For mutable references, we write safe values to the pointer *)
        let* rv, _ =
          Summary.run_producer Summary.Typed.Expr.Subst.empty (Option.get summ)
        in
        State.store (Summary.Value.as_ptr ret) ty rv
    | _ -> State.SM.Result.ok ()
  in
  (ty, ret, arg_ptrs)

let branch drops wrapper =
  (* Obtain the result from the executing the function call *)
  let** ty, ret, arg_ptrs = wrapper in
  (* Drop the return value *)
  let drop_ret () =
    exec_drop drops ty ~none:(State.SM.Result.ok ()) ~some:(alloc ty ret)
  in
  (* Drop a reference argument *)
  let drop_ptr ty ptr () =
    exec_drop drops ty
      ~none:(State.free (Summary.Value.as_ptr ptr))
      ~some:(State.SM.Result.ok ptr)
  in
  let lift_nondet ty ret =
    let* nondet = Summary.Value.nondet ty |> State.SM.lift in
    let* () = State.SM.assume [ Summary.Value.sem_eq nondet ret ] in
    State.SM.Result.ok (ty, nondet)
  in
  (* For each reference, we create an execution branch that returns the stored
     value and drops everything else, including the return value *)
  let rec get_branches ?(acc = []) ?(drops = State.SM.Result.ok ()) = function
    | [] ->
        (* Case 0: we learn from the return value, the rest has been dropped *)
        let branch () =
          let** () = drops in
          lift_nondet ty ret
        in
        branch :: acc
    | (ty, ptr) :: arg_ptrs ->
        (* Case 1: we learn from this reference and drop the rest *)
        let branch () =
          let** () = drops in
          let** ret = State.load (Summary.Value.as_ptr ptr) ty in
          let** () = State.free (Summary.Value.as_ptr ptr) in
          let** () =
            ListLabels.fold_left arg_ptrs ~init:(drop_ret ())
              ~f:(fun st (ty, ptr) ->
                let** () = st in
                drop_ptr ty ptr ())
          in
          lift_nondet ty ret
        in
        (* Case 2: we learn nothing from this reference, so we drop it *)
        let drops =
          let** () = drops in
          drop_ptr ty ptr ()
        in
        (* Keep case 1 in the result and proceed with the state from case 2 *)
        get_branches arg_ptrs ~acc:(branch :: acc) ~drops
  in
  get_branches arg_ptrs |> State.SM.branches

let make drops (fun_decl : UllbcAst.fun_decl) : t * Types.ty list =
  let tys =
    let sign = fun_decl.signature in
    let tys =
      List.map (function Types.TRef (_, ty, _) | ty -> ty) sign.inputs
    in
    match sign.output with TRef (_, ty, RMut) -> ty :: tys | _ -> tys
  in
  let fn summs = call fun_decl summs |> branch drops in
  ({ name = fun_decl.item_meta.name; fn }, tys)

let exec ~fuel (wrapper : t) summs =
  (* Symbolically execute the wrapped function call *)
  State.SM.Result.run_with_state ~state:State.empty (wrapper.fn summs)
  |> Rustsymex.run ~stats:Caller ~mode:UX ~fuel
  |> Monad.ResultM.fold_list ~init:[] ~f:(fun summs -> function
    (* Values that borrow from the arguments are not supported yet, as the
       arguments are dropped after the call: no summary is inferred *)
    | ( Compo_res.Ok
          ((Types.TAdt { generics = { regions = _ :: _; _ }; _ }, _), _),
        _ ) ->
        Ok summs
    (* Successful termination: a new summary can been inferred *)
    | Compo_res.Ok ((ty, ret), state), pcs ->
        let open Result.Syntax in
        let+ summ = Summary.make ret state pcs in
        (ty, summ) :: summs
    (* Panics are safe, but they do not produce a summary either *)
    | Compo_res.Error ((e, _), _), _
      when [%matches?
             `Panic _ | `PanicCheck _ | `FailedAssert _ | `UnwindTerminate]
             e ->
        Ok summs
    (* Unsuccessful termination: found a type unsoundness *)
    | _ -> Result.error `TypeUnsound)
