open Soteria_rust_lib
module State = Summary.State
module Interp = Interp.Make (State)
open State.SM.Syntax
open Charon

let exec_fun ~args fun_decl =
  let* state = State.SM.get_state () in
  let** ret, state =
    State.SM.lift @@ Interp.exec_fun_compo ~args ~state fun_decl
  in
  let+ () = State.SM.set_state state in
  Compo_res.ok ret

module Symok = struct
  let unwrap res =
    State.SM.map
      (function Compo_res.Ok v -> v | _ -> failwith "Expected Ok in wrapper")
      res

  let load ret ty = State.load (Summary.Value.as_ptr ret) ty |> unwrap
  let store ret ty rv = State.store (Summary.Value.as_ptr ret) ty rv |> unwrap
  let free ret = State.free (Summary.Value.as_ptr ret) |> unwrap

  let alloc ty rv =
    let* ptr = State.alloc_ty ty |> unwrap in
    let+ () = store ptr ty rv in
    ptr

  let exec_drop drops ty ~none ~some =
    match ty with
    | Types.TAdt { id; _ } -> (
        match Types.TypeDeclId.Map.find_opt id drops with
        | Some fun_decl ->
            let* ret = some in
            let* _ = exec_fun fun_decl ~args:[ ret ] |> unwrap in
            free ret
        | None -> none)
    | _ -> none
end

type t =
  Summary.t list ->
  ( Types.ty * Summary.Value.t,
    Error.with_trace * Interp.StateM.st,
    State.syn list )
  State.SM.Result.t

let call (fun_decl : UllbcAst.fun_decl) summs =
  let ty = fun_decl.signature.output in
  let if_rmut ~then_ ~else_ =
    let rmut_ty = match ty with TRef (_, ty, RMut) -> Some ty | _ -> None in
    Option.fold rmut_ty ~none:else_ ~some:then_
  in
  let summ = if_rmut ~then_:(fun _ -> Some (List.hd summs)) ~else_:None in
  let summs = if_rmut ~then_:(fun _ -> List.tl summs) ~else_:summs in
  (* Check reference arguments and allocate values on heap *)
  let* args, arg_ptrs, subst =
    ListLabels.fold_left2 summs fun_decl.signature.inputs
      ~init:(State.SM.return ([], [], Summary.Typed.Expr.Subst.empty))
      ~f:(fun acc summ ty ->
        let* args, arg_ptrs, subst = acc in
        let* arg, subst = Summary.run_producer subst summ in
        match ty with
        | Types.TRef (_, ty, _) ->
            let+ ptr = Symok.alloc ty arg in
            (ptr :: args, (ty, ptr) :: arg_ptrs, subst)
        | _ -> State.SM.return (arg :: args, arg_ptrs, subst))
  in
  let args = List.rev args in
  (* Symbolically execute the function call *)
  let** ret = exec_fun fun_decl ~args in
  (* Handle the return value if it is a reference *)
  let+ () =
    match ty with
    | TRef (_, ty, RShared) ->
        (* For shared references, we simply read from the pointer *)
        let+ _ = Symok.load ret ty in
        ()
    | TRef (_, ty, RMut) ->
        (* For mutable references, we write safe values to the pointer *)
        let* rv, _ = Summary.run_producer subst (Option.get summ) in
        Symok.store ret ty rv
    | _ -> State.SM.return ()
  in
  Compo_res.Ok (ty, ret, arg_ptrs)

let branch drops wrapper =
  (* Obtain the result from the executing the function call *)
  let** ty, ret, arg_ptrs = wrapper in
  (* Drop the return value *)
  let drop_ret () =
    Symok.exec_drop drops ty ~none:(State.SM.return ())
      ~some:(Symok.alloc ty ret)
  in
  (* Drop a reference argument *)
  let drop_ptr ty ptr () =
    Symok.exec_drop drops ty ~none:(Symok.free ptr) ~some:(State.SM.return ptr)
  in
  let lift_nondet ty ret =
    let* nondet = Summary.Value.nondet ty |> State.SM.lift in
    let* () = State.SM.assume [ Summary.Value.sem_eq nondet ret ] in
    State.SM.Result.ok (ty, nondet)
  in
  (* For each reference, we create an execution branch that returns the stored
     value and drops everything else, including the return value *)
  let rec get_branches ?(acc = []) ?(drops = State.SM.return ()) = function
    | [] ->
        (* Case 0: we learn from the return value, the rest has been dropped *)
        let branch () =
          let* () = drops in
          lift_nondet ty ret
        in
        branch :: acc
    | (ty, ptr) :: arg_ptrs ->
        (* Case 1: we learn from this reference and drop the rest *)
        let branch () =
          let* () = drops in
          let* ret = Symok.load ptr ty in
          let* () = Symok.free ptr in
          let* () =
            ListLabels.fold_left arg_ptrs ~init:(drop_ret ())
              ~f:(fun st (ty, ptr) -> State.SM.bind (drop_ptr ty ptr) st)
          in
          lift_nondet ty ret
        in
        (* Case 2: we learn nothing from this reference, so we drop it *)
        let drops =
          let* () = drops in
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
  let wrapper summs = call fun_decl summs |> branch drops in
  (wrapper, tys)

let exec ~fuel (wrapper : t) summs =
  (* Symbolically execute the wrapped function call *)
  State.SM.Result.run_with_state ~state:State.empty (wrapper summs)
  |> Rustsymex.run ~stats:Caller ~mode:UX ~fuel
  |> Result.fold_list ~init:[] ~f:(fun summs -> function
    (* Successful termination: a new summary can been inferred *)
    | Compo_res.Ok ((ty, ret), state), pcs ->
        let open Result.Syntax in
        let+ summ = Summary.make ret state pcs in
        (ty, summ) :: summs
    (* Unsuccessful termination: found a type unsoundness *)
    | _ -> Result.error `TypeUnsound)
