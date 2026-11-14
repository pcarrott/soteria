open Charon
open Soteria_rust_lib
module State = Summary.State
module Typed = Summary.Typed

(** {2 Which types need drop} *)

type needs_drop = No | Yes | Unknown

module Ty_map = Map.Make (struct
  type t = Types.ty

  let compare = Types.compare_ty
  let pp = Types.pp_ty
  let show = Types.show_ty
end)

(* The drop glues and [Drop] impls of the crate. Charon only translates the drop
   glue of the types that the crate drops. The drop of other types is run from
   their structure instead. *)
type t = {
  glues : UllbcAst.fun_decl Types.TypeDeclId.Map.t;
  impls : UllbcAst.fun_decl Types.TypeDeclId.Map.t;
      (** The [Drop] impls of the crate *)
  needs_drop : needs_drop Ty_map.t;
      (** Whether dropping values of a type that may be dropped does anything *)
}

let trait_lang_item ({ src; _ } : UllbcAst.fun_decl) =
  match src with
  | TraitImplFun (_, decl, _, _) ->
      (Crate.get_trait_decl decl).item_meta.lang_item
  | _ -> None

let is_glue fun_decl = trait_lang_item fun_decl = Some RustcLangItemDestruct
let is_impl fun_decl = trait_lang_item fun_decl = Some RustcLangItemDrop

(* Whether a function is a drop glue or a [Drop] impl *)
let is_drop fun_decl = is_glue fun_decl || is_impl fun_decl

let join_drop x y =
  match (x, y) with
  | Unknown, _ | _, Unknown -> Unknown
  | Yes, _ | _, Yes -> Yes
  | No, No -> No

let adt_name (adt : Types.type_decl_ref) =
  Fmt.to_to_string Crate.pp_name (Crate.get_adt_raw adt.id).item_meta.name

let is_manually_drop adt =
  adt_name adt = "core::mem::manually_drop::ManuallyDrop"

let is_non_null adt = adt_name adt = "core::ptr::non_null::NonNull"
let is_unsized ty = not ([%matches? Layout.NoneKind] (Layout.dst_kind ty))

let adt_field_tys (adt : Types.type_decl_ref) =
  let tys fields = List.map (fun (f : Types.field) -> f.field_ty) fields in
  match (Crate.get_adt adt).kind with
  | Struct fields -> tys fields
  | Enum variants ->
      List.concat_map (fun (v : Types.variant) -> tys v.fields) variants
  | _ -> []

(* Whether dropping a value of type [ty] does anything. A type of another crate
   may own its pointers through a [Drop] impl that was not translated, which is
   [Unknown]; the same pointer in a type of this crate does not own anything. *)
let max_depth = 64

let rec needs_drop ?(seen = []) ?(foreign = false) drops (ty : Types.ty) =
  let aux = needs_drop ~seen:(ty :: seen) in
  let all ~foreign tys =
    List.fold_left (fun acc ty -> join_drop acc (aux ~foreign drops ty)) No tys
  in
  if List.compare_length_with seen max_depth > 0 then Unknown
  else if List.exists (Types.equal_ty ty) seen then No
  else
    match ty with
    | TAdt { builtin = Some TTuple; generics = { types; _ }; _ } ->
        all ~foreign types
    | TAdt { id; builtin = Some TBox; _ }
      when Types.TypeDeclId.Map.mem id drops.glues ->
        Yes
    | TAdt { builtin = Some TBox; generics = { types; _ }; _ } ->
        let pointee = List.hd types in
        if is_unsized pointee then Unknown
        else join_drop Yes (aux ~foreign drops pointee)
    | TAdt { builtin = Some _; _ } -> No
    | TAdt adt when is_manually_drop adt -> No
    | TAdt adt when is_non_null adt -> if foreign then Unknown else No
    | TAdt adt ->
        let decl = Crate.get_adt_raw adt.id in
        let foreign =
          if decl.item_meta.is_local then false
          else foreign || not (Crate.is_enum adt)
        in
        let fields = all ~foreign (adt_field_tys adt) in
        if Types.TypeDeclId.Map.mem adt.id drops.impls then Yes
        else if
          (* The glue of a type is generic: it may do nothing for this instance,
             but it does settle what we could not see. *)
          fields = Unknown && Types.TypeDeclId.Map.mem adt.id drops.glues
        then Yes
        else fields
    | TArray (ty, _, _) -> aux ~foreign drops ty
    | TRawPtr _ -> if foreign then Unknown else No
    | TTraitType _ | TDynTrait _ | TSlice _ -> Unknown
    (* A value of a type variable is opaque, and owned until dropped *)
    | TVar (Free _) -> Yes
    | _ -> No

(* The types that a value of type [ty] is dropped through, when it needs drop *)
let dropped_tys (ty : Types.ty) =
  match ty with
  | TAdt { builtin = Some TTuple; generics = { types; _ }; _ } -> types
  | TAdt { builtin = Some TBox; generics = { types; _ }; _ } ->
      [ List.hd types ]
  | TAdt { builtin = Some _; _ } -> []
  | TAdt adt -> adt_field_tys adt
  | TArray (ty, _, _) -> [ ty ]
  | _ -> []

(* The drops of the crate, where [needs_drop] is computed for the types of the
   signatures of [fun_decls] and the types they are dropped through, up to the
   depth at which [needs_drop] gives up *)
let get (fun_decls : UllbcAst.fun_decl list) =
  let add (fun_decl : UllbcAst.fun_decl) map =
    match List.hd fun_decl.signature.inputs with
    | TRef (_, TAdt { id; _ }, _) -> Types.TypeDeclId.Map.add id fun_decl map
    | _ -> map
  in
  let empty = Types.TypeDeclId.Map.empty in
  let drops =
    Types.FunDeclId.Map.fold
      (fun _ fun_decl drops ->
        if is_glue fun_decl then { drops with glues = add fun_decl drops.glues }
        else if is_impl fun_decl then
          { drops with impls = add fun_decl drops.impls }
        else drops)
      (Crate.get_crate ()).fun_decls
      { glues = empty; impls = empty; needs_drop = Ty_map.empty }
  in
  (* Breadth-first, so that a type is reached at its smallest depth *)
  let rec visit depth tys map =
    if List.is_empty tys || depth > max_depth then map
    else
      let map, next =
        List.fold_left
          (fun (map, next) ty ->
            if Ty_map.mem ty map then (map, next)
            else
              let needs = needs_drop drops ty in
              let next = if needs = Yes then dropped_tys ty @ next else next in
              (Ty_map.add ty needs map, next))
          (map, []) tys
      in
      visit (depth + 1) next map
  in
  let tys =
    List.concat_map
      (fun (fun_decl : UllbcAst.fun_decl) ->
        let Types.{ inputs; output; _ } = fun_decl.signature in
        List.concat_map
          (function
            | Types.TRef (_, pointee, _) as ty -> [ ty; pointee ] | ty -> [ ty ])
          (output :: inputs))
      fun_decls
  in
  { drops with needs_drop = visit 0 tys Ty_map.empty }

(* Types beyond the depth at which [needs_drop] gives up are not computed *)
let needs_drop drops ty =
  Option.value ~default:Unknown (Ty_map.find_opt ty drops.needs_drop)

(** {2 Running drops} *)

module Make (M : sig
  include
    Soteria.Sym_states.State_monad.S
      with module Value = Rustsymex.Value
       and type st = State.SM.st * Summary.Value.t list

  val lift : 'a Rustsymex.t -> 'a t
  val lift_state : 'a State.SM.t -> 'a t
  val forget : Summary.Value.t -> unit t

  val box :
    Types.ty ->
    Summary.Value.t ->
    (Summary.Value.t, Error.with_trace, State.syn list) Result.t

  val free :
    Summary.Value.t -> (unit, Error.with_trace, State.syn list) Result.t

  val exec_fn_ptr :
    Types.fn_ptr ->
    Summary.Value.t list ->
    (Summary.Value.t, Error.with_trace, State.syn list) Result.t
end) =
struct
  open M.Syntax

  let ( let**^ ) x f = M.Result.bind f (M.lift_state x)
  let ( let++^ ) x f = M.Result.map f (M.lift_state x)

  (* Executes [<ty as Destruct>::drop_glue(ptr)] like a drop desugared by
     Charon, where [drop_glue] is the drop glue of the ADT [ty]: its impl is
     instantiated with the generics of [ty], which carry the [Destruct] impls of
     the type parameters. *)
  let exec_drop_glue (drop_glue : UllbcAst.fun_decl) (ty : Types.ty) ptr =
    match (drop_glue.src, ty) with
    | ( TraitImplFun (impl_ref, trait_decl_ref, method_id, _),
        TAdt { generics; _ } ) ->
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
        M.exec_fn_ptr fn_ptr [ ptr ]
    | _ -> failwith "Expected the drop glue of an ADT"

  let layout_of ty =
    M.lift_state
    @@ State.with_pointers_sym
    @@ State.Sptr.DecayMap.SM.lift_layout
         (let open Layout.SM.Syntax in
          let*^ trace = Rustsymex.get_trace () in
          let+- err = Layout.layout_of ty in
          Error.log_at trace err;
          Error.decorate trace err)

  (* Drops the value at [ptr] in place, like [std::ptr::drop_in_place]: its
     [Drop] impl runs first, then its fields are dropped in place, so the value
     keeps its address and its allocation is alive while it is dropped. [boxes]
     holds the boxes whose contents are being dropped. *)
  let rec drop_place ~boxes drops (ty : Types.ty) ptr =
    match needs_drop drops ty with
    | No -> M.Result.ok ()
    | Unknown ->
        let**^ v = State.load (Summary.Value.as_ptr ptr) ty in
        let+ () = M.forget v in
        Compo_res.ok ()
    | Yes -> (
        match ty with
        | TAdt { id; _ } when Types.TypeDeclId.Map.mem id drops.glues ->
            let++ _ =
              exec_drop_glue (Types.TypeDeclId.Map.find id drops.glues) ty ptr
            in
            ()
        | TAdt { builtin = Some TBox; generics = { types; _ }; _ } ->
            let**^ v = State.load (Summary.Value.as_ptr ptr) ty in
            let box_ptr, _, _ = Value_codec.unwrap_box (Typed.cast v) in
            drop_box ~boxes drops (List.hd types) (Summary.Value.of_ptr box_ptr)
        | TAdt { id; _ } when Types.TypeDeclId.Map.mem id drops.impls ->
            let** _ =
              exec_drop_glue (Types.TypeDeclId.Map.find id drops.impls) ty ptr
            in
            drop_fields ~boxes drops ty ptr
        | TVar (Free id) ->
            let**^ v = State.load (Summary.Value.as_ptr ptr) ty in
            M.lift_state @@ State.drop_opaque id v
        | _ -> drop_fields ~boxes drops ty ptr)

  (* Drops the fields of the value at [ptr] in place, those of its current
     variant for an enum *)
  and drop_fields ~boxes drops ty ptr =
    let ptr_f = Summary.Value.as_ptr ptr in
    let** layout = layout_of ty in
    let** variant =
      match (layout.fields, ty) with
      | Enum _, TAdt adt ->
          let**^ discr = State.load_discriminant ptr_f ty in
          let* variant =
            M.lift (Value_codec.variant_for_discriminant discr adt)
          in
          M.Result.ok (Some variant.id)
      | _ -> M.Result.ok None
    in
    match layout.fields with
    (* The type is uninhabited *)
    | Primitive -> M.Result.ok ()
    | _ ->
        Value_codec.iter_fields ?variant layout ty
        |> Iter.to_list
        |> M.Result.fold_list ~init:() ~f:(fun () (field_ty, ofs) ->
            let**^ field_ptr = State.Sptr.offset ofs (Typed.Ptr.ptr_of ptr_f) in
            drop_place ~boxes drops field_ty
              (Summary.Value.of_ptr (Typed.Ptr.of_ptr_t field_ptr)))

  (* Drops the contents of a box in place, then frees it. A box whose contents
     are already being dropped is part of a cycle of boxes, whose drop recurses
     until the stack overflows: the program aborts, which is not undefined
     behaviour. *)
  and drop_box ~boxes drops ty ptr =
    (* Pointers to the same address may differ in their borrow tags *)
    let same_address p q =
      let p = Typed.Ptr.ptr_of (Summary.Value.as_ptr p) in
      let q = Typed.Ptr.ptr_of (Summary.Value.as_ptr q) in
      Typed.Infix.(
        Typed.Ptr.loc p
        ==@ Typed.Ptr.loc q
        &&@ (Typed.Ptr.ofs p ==@ Typed.Ptr.ofs q))
    in
    let in_cycle =
      List.fold_left
        (fun acc box -> Typed.Infix.(acc ||@ same_address box ptr))
        Typed.v_false boxes
    in
    M.branch_on in_cycle ~then_:M.vanish ~else_:(fun () ->
        let** () = drop_place ~boxes:(ptr :: boxes) drops ty ptr in
        M.free ptr)

  (* Drops a value like [std::mem::drop]: the value is moved to a fresh place,
     where it is dropped *)
  let drop_value drops ty v =
    match needs_drop drops ty with
    | No -> M.Result.ok ()
    | Unknown ->
        let* () = M.forget v in
        M.Result.ok ()
    | Yes ->
        let** ptr = M.box ty v in
        let** () = drop_place ~boxes:[] drops ty ptr in
        M.free ptr
end
