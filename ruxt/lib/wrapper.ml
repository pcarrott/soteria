open Soteria.Soteria_std
open Soteria_rust_lib
module State = Summary.State
module Interp = Interp.Make (State)
open Charon

(* [State.SM] over the witness monad: the heap state is run before the witness
   is, and the [^] operators of its syntax lift computations of [State.SM] *)
module SM = struct
  include
    Soteria.Sym_states.State_monad.Make
      (Witness.WM)
      (struct
        type t = State.SM.st
      end)

  let with_call fun_decl = lift (Witness.WM.with_call fun_decl)
  let ret_var = lift Witness.WM.ret_var
  let add_arg arg = lift (Witness.WM.add_arg arg)
  let add_stmt stmt = lift (Witness.WM.add_stmt stmt)
  let set_result var = lift (Witness.WM.set_result var)
  let lift_state (x : 'a State.SM.t) : 'a t = fun st -> Witness.WM.lift @@ x st
  let lift x = lift (Witness.WM.lift x)

  module Syntax = struct
    include Syntax

    let ( let**^ ) x f = Result.bind f (lift_state x)
    let ( let++^ ) x f = Result.map f (lift_state x)
  end
end

open SM.Syntax

(* Runs an interpreter computation from the current state, dropping the state
   the interpreter pairs with its errors *)
let lift_interp f =
  let* state = SM.get_state () in
  let** ret, state =
    SM.lift @@ Rustsymex.map (Compo_res.map_error fst) @@ f ~state
  in
  let+ () = SM.set_state state in
  Compo_res.ok ret

let exec_fun ~args fun_decl = lift_interp (Interp.exec_fun_compo ~args fun_decl)

(* Produces the summary [summ], returning its value along with its witness *)
let run_producer (summ : Summary.t) : (Summary.Value.t * Witness.t) SM.t =
  let* state = SM.get_state () in
  let* (ret, witness, state), _ =
    let subst = Summary.Typed.Expr.Subst.empty in
    SM.lift @@ Rustsymex.Producer.run ~subst @@ Summary.produce summ state
  in
  let+ () = SM.set_state state in
  (ret, witness)

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
  let**^ ptr = State.alloc_ty ty in
  let++^ () = State.store (Summary.Value.as_ptr ptr) ty v in
  ptr

let exec_drop drops ty var ~none ~some =
  match ty with
  | Types.TAdt { id; _ } -> (
      match Types.TypeDeclId.Map.find_opt id drops with
      | Some fun_decl ->
          let** ptr = some in
          let* () = SM.add_stmt (Witness.Drop var) in
          let** _ = exec_drop_glue fun_decl ty ptr in
          SM.lift_state @@ State.free (Summary.Value.as_ptr ptr)
      | None -> none)
  | _ -> none

type fn =
  Summary.t list ->
  (Types.ty * Summary.Value.t, Error.with_trace, State.syn list) SM.Result.t

type t = { name : Types.name; fn : fn }

let name { name; _ } = name

let call (fun_decl : UllbcAst.fun_decl) summs =
  let ty = fun_decl.signature.output in
  let* () = SM.with_call fun_decl in
  (* Check reference arguments and allocate values on heap *)
  let** args, arg_ptrs =
    let inputs =
      match ty with TRef (_, _, RMut) -> List.tl summs | _ -> summs
    in
    ListLabels.fold_left2 inputs fun_decl.signature.inputs
      ~init:(SM.Result.ok ([], []))
      ~f:(fun acc summ ty ->
        let** args, arg_ptrs = acc in
        let* arg, src = run_producer summ in
        match ty with
        | Types.TRef (_, ty, ref_kind) ->
            let** ptr = alloc ty arg in
            let* () = SM.add_arg (Some ref_kind, src) in
            let var = Witness.result src in
            SM.Result.ok (ptr :: args, (ty, ptr, var) :: arg_ptrs)
        | _ ->
            let* () = SM.add_arg (None, src) in
            SM.Result.ok (arg :: args, arg_ptrs))
  in
  let args = List.rev args in
  (* Symbolically execute the function call *)
  let** ret = exec_fun fun_decl ~args in
  (* Handle the return value if it is a reference *)
  let++ () =
    let* ret_var = SM.ret_var in
    match ty with
    | TRef (_, ty, RShared) ->
        (* For shared references, we simply read from the pointer *)
        let* () = SM.add_stmt (Witness.Read ret_var) in
        let++^ _ = State.load (Summary.Value.as_ptr ret) ty in
        ()
    | TRef (_, ty, RMut) ->
        (* For mutable references, we write safe values to the pointer *)
        let* rv, src = run_producer (List.hd summs) in
        let* () = SM.add_stmt (Witness.Write (ret_var, src)) in
        SM.lift_state @@ State.store (Summary.Value.as_ptr ret) ty rv
    | _ -> SM.Result.ok ()
  in
  (ty, ret, arg_ptrs)

let branch drops wrapper =
  (* Obtain the result from the executing the function call *)
  let** ty, ret, arg_ptrs = wrapper in
  (* Drop the return value *)
  let drop_ret () =
    let* ret_var = SM.ret_var in
    exec_drop drops ty ret_var ~none:(SM.Result.ok ()) ~some:(alloc ty ret)
  in
  (* Drop a reference argument *)
  let drop_ptr () (ty, ptr, var) =
    exec_drop drops ty var
      ~none:(SM.lift_state @@ State.free (Summary.Value.as_ptr ptr))
      ~some:(SM.Result.ok ptr)
  in
  let lift_nondet ty ret =
    let* nondet = SM.lift @@ Summary.Value.nondet ty in
    let* () = SM.assume [ Summary.Value.sem_eq nondet ret ] in
    SM.Result.ok (ty, nondet)
  in
  (* For each reference, we create an execution branch that returns the stored
     value and drops everything else, including the return value *)
  let rec get_branches ?(acc = []) ?(drops = SM.Result.ok ()) = function
    | [] ->
        (* Case 0: we learn from the return value, the rest has been dropped *)
        let branch () =
          let** () = drops in
          lift_nondet ty ret
        in
        branch :: acc
    | (ty, ptr, var) :: arg_ptrs ->
        (* Case 1: we learn from this reference and drop the rest *)
        let branch () =
          let** () = drops in
          let* () = SM.set_result var in
          let**^ ret = State.load (Summary.Value.as_ptr ptr) ty in
          let**^ () = State.free (Summary.Value.as_ptr ptr) in
          let** () = SM.Result.fold_list arg_ptrs ~init:() ~f:drop_ptr in
          let** () = drop_ret () in
          lift_nondet ty ret
        in
        (* Case 2: we learn nothing from this reference, so we drop it *)
        let drops =
          let** () = drops in
          drop_ptr () (ty, ptr, var)
        in
        (* Keep case 1 in the result and proceed with the state from case 2 *)
        get_branches arg_ptrs ~acc:(branch :: acc) ~drops
  in
  get_branches arg_ptrs |> SM.branches

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
  wrapper.fn summs
  |> SM.Result.run_with_state ~state:State.empty
  |> Witness.WM.run_with_witness ~witness:Witness.empty
  |> Rustsymex.run ~stats:Caller ~mode:UX ~fuel
  |> Monad.ResultM.fold_list ~init:[] ~f:(fun summs -> function
    (* Values that borrow from the arguments are not supported yet, as the
       arguments are dropped after the call: no summary is inferred *)
    | ( ( Compo_res.Ok
            ((Types.TAdt { generics = { regions = _ :: _; _ }; _ }, _), _),
          _ ),
        _ ) ->
        Ok summs
    (* Successful termination: a new summary can been inferred *)
    | (Compo_res.Ok ((ty, ret), state), witness), pcs ->
        let open Result.Syntax in
        let+ summ =
          Summary.make ret witness state pcs
          |> Result.map_error (fun `MemoryLeak ->
              `MemoryLeak (Witness.program ~pcs witness))
        in
        (ty, summ) :: summs
    (* Panics are safe, but they do not produce a summary either *)
    | (Compo_res.Error ((e, _), _), _), _
      when [%matches?
             `Panic _ | `PanicCheck _ | `FailedAssert _ | `UnwindTerminate]
             e ->
        Ok summs
    (* Unsuccessful termination: found a type unsoundness *)
    | ((Compo_res.Error _ | Missing _), witness), pcs ->
        Result.error (`TypeUnsound (Witness.program ~pcs witness)))
