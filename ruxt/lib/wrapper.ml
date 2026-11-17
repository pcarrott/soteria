open Charon
open Soteria.Soteria_std
module Interp_ = Interp
module Config_ = Config
open Soteria_rust_lib
module Interp = Interp_
module Config = Config_
module State = Summary.State
module SM = Interp.StateM
open SM.Syntax

type fn =
  Summary.t list ->
  (Types.ty * Summary.Value.t, Error.with_trace, State.syn list) SM.Result.t

type t = { name : Types.name; fn : fn }

let name { name; _ } = name

let call (fun_decl : UllbcAst.fun_decl) summs =
  let* () = SM.with_call fun_decl in
  (* Check reference arguments and allocate values on heap *)
  let** args, arg_ptrs =
    ListLabels.fold_left2 summs fun_decl.signature.inputs
      ~init:(SM.Result.ok ([], []))
      ~f:(fun acc summ ty ->
        let** args, arg_ptrs = acc in
        let* arg, src = Interp.run_producer summ in
        match ty with
        | Types.TRef (_, ty, ref_kind) ->
            let** ptr = SM.box ty arg in
            let var = Witness.result src in
            let* () = SM.add_arg (Some ref_kind, src) in
            SM.Result.ok (ptr :: args, (ty, ptr, var) :: arg_ptrs)
        | _ ->
            let* () = SM.add_arg (None, src) in
            SM.Result.ok (arg :: args, arg_ptrs))
  in
  let args = List.rev args in
  (* Symbolically execute the function call *)
  let++ ret = Interp.exec_fun fun_decl ~args in
  (ret, arg_ptrs)

let return drops ty summ wrapper =
  let** ret, arg_ptrs = wrapper in
  (* The return value must be valid. It is checked before the arguments are
     dropped, as its references may borrow from them; the values behind
     reference arguments are checked when they are read. *)
  let**^ () =
    State.with_loc_err ~trace:"Return value validity" () (fun () ->
        State.check_validity ~check_refs:false ty ret)
  in
  let++ () =
    match ty with
    | Types.TRef (_, ty, RShared) ->
        (* For shared references, we read from the pointer *)
        Interp.exec_read ty ret
    | TRef (_, ty, RMut) ->
        (* For mutable references, we write safe values to the pointer *)
        let* rv, src = Interp.run_producer @@ Option.get summ in
        Interp.exec_write drops ty ret rv src
    | _ -> SM.Result.ok ()
  in
  (ret, arg_ptrs)

(* The layouts of the type variables in the heap state [st] *)
let layouts st =
  Sptr.DecayMap.get_layouts (State.of_opt st).pointers
  |> Layout.Ctx.of_opt
  |> Layout.Ctx.syntactic_bindings
  |> List.of_seq

let branch drops ty wrapper =
  (* Obtain the result from the executing the function call *)
  let** ret, arg_ptrs = wrapper in
  (* Drop the return value *)
  let drop_ret () = Interp.exec_drop drops ty (`Value ret) in
  (* Drop a reference argument *)
  let drop_ptr (ty, ptr, var) = Interp.exec_drop drops ty ~var (`Place ptr) in
  (* The inferred safe value, whose validity has been checked, constrains the
     allocation information of its pointers. When implications between summaries
     can learn all of its variables, it is kept as is, so that its structure,
     and the locations of the blocks it points to, can be learned. Otherwise, it
     is tied to a fresh symbolic value, which can always be learned.

     This is decided for the whole value: a value that points to blocks of the
     heap but also holds a part that cannot be learned is tied to a fresh value,
     which hides the locations of its blocks, so implications that consume them
     fail. *)
  let assume_valid ty ret =
    (* The layouts of the type variables are learned before the value *)
    let* st = SM.get_heap () in
    let known = List.map snd (layouts st) in
    if Summary.Value.is_learnable ~known ret then
      let* () = SM.lift @@ Summary.Value.assume_valid ty ret in
      SM.Result.ok (ty, ret)
    else
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
          assume_valid ty ret
        in
        branch :: acc
    | (ty, ptr, var) :: arg_ptrs ->
        (* Case 1: we learn from this reference and drop the rest *)
        let branch () =
          let** () =
            (* The return value goes first: it may borrow from the arguments *)
            let** () = drop_ret () in
            let** () = drops in
            SM.Result.fold_list arg_ptrs ~init:() ~f:(fun () -> drop_ptr)
          in
          let* () = SM.set_result var in
          let** ret = SM.unbox ty ptr in
          assume_valid ty ret
        in
        (* Case 2: we learn nothing from this reference, so we drop it *)
        let drops =
          let** () = drops in
          drop_ptr (ty, ptr, var)
        in
        (* Keep case 1 in the result and proceed with the state from case 2 *)
        get_branches arg_ptrs ~acc:(branch :: acc) ~drops
  in
  get_branches arg_ptrs |> SM.branches

let make drops (fun_decl : UllbcAst.fun_decl) : t * Types.ty list =
  let Types.{ inputs; output = ty; _ } = fun_decl.signature in
  let tys =
    let tys = List.map (function Types.TRef (_, ty, _) | ty -> ty) inputs in
    match ty with TRef (_, ty, RMut) -> ty :: tys | _ -> tys
  in
  let fn summs =
    let summ, summs =
      if [%matches? TRef (_, _, RMut)] ty then
        (Some (List.hd summs), List.tl summs)
      else (None, summs)
    in
    call fun_decl summs |> return drops ty summ |> branch drops ty
  in
  ({ name = fun_decl.item_meta.name; fn }, tys)

(* The type variables whose layout may not be zero-sized on the current path.
   Values of a type variable only lose their identity in memory on paths where
   its size is zero. *)
let sized_ty_vars () : Types.type_var_id list SM.t =
  let open Summary.Typed.Infix in
  let* st = SM.get_heap () in
  SM.lift
  @@ Rustsymex.fold_list ~init:[] (layouts st) ~f:(fun acc (id, size) ->
      Rustsymex.map
        (fun zero -> if zero then acc else id :: acc)
        (Rustsymex.assert_ (size ==@ Summary.Typed.Syntax.Usize.(0s))))

let exec ~fuel ({ fn; name = _ } : t) summs =
  (* Symbolically execute the wrapped function call *)
  SM.Result.run
    (let** res = fn summs in
     (* Only the leak check needs the sized type variables *)
     let* sized =
       if (Config.get ()).ignore_leaks then SM.return [] else sized_ty_vars ()
     in
     SM.Result.ok (res, sized))
  |> Rustsymex.run ~stats:Caller ~mode:UX ~fuel
  |> Monad.ResultM.fold_list ~init:[] ~f:(fun summs -> function
    (* Values that borrow from the arguments are not supported yet, as the
       arguments are dropped after the call: no summary is inferred *)
    | ( ( ( Compo_res.Ok
              ((Types.TAdt { generics = { regions = _ :: _; _ }; _ }, _), _),
            _ ),
          _ ),
        _ ) ->
        Ok summs
    (* Successful termination: a new summary can been inferred *)
    | ((Compo_res.Ok ((ty, ret), sized), (state, forgotten)), witness), pcs ->
        let open Result.Syntax in
        let+ summ =
          Summary.make ~ty ~sized ret witness state ~forgotten pcs
          |> Result.map_error (fun `MemoryLeak ->
              `MemoryLeak
                (Witness.program ~pcs ~layouts:(layouts state) witness))
        in
        (ty, summ) :: summs
    (* Panics are safe, but they do not produce a summary either *)
    | ((Compo_res.Error (e, _), _), _), _
      when [%matches?
             `Panic _ | `PanicCheck _ | `FailedAssert _ | `UnwindTerminate]
             e ->
        Ok summs
    (* Unsuccessful termination: found a type unsoundness *)
    | ((Compo_res.(Error _ | Missing _), (state, _)), witness), pcs ->
        Result.error
          (`TypeUnsound (Witness.program ~pcs ~layouts:(layouts state) witness)))
