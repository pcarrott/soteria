open Charon
open Svalue
open Typed.Infix
open Typed.Syntax
open Compo_res
module BV = Typed.BV
open Rustsymex
open Rustsymex.Result
open Rustsymex.Syntax
open Common.Charon_util

(* Import types *)
include Layout_common

module Session = struct
  (** Cache of (type or variant) -> layout *)
  type cache = (Types.ty, t) Hashtbl.t

  type _ Effect.t += Get_cache : cache Effect.t

  let get_cache () = Effect.perform Get_cache

  let with_layout_cache f =
    let open Effect.Deep in
    let cache : (Types.ty, t) Hashtbl.t = Hashtbl.create 128 in
    try f () with effect Get_cache, k -> continue k cache

  let get_or_compute_cached_layout ty f =
    let cache = get_cache () in
    match Hashtbl.find_opt cache ty with
    | Some layout -> ok layout
    | None -> (
        let* gen_layout = Poly.get_layout ty in
        match gen_layout with
        | Some layout -> ok layout
        | None ->
            let** layout = f () in
            let is_concrete = Iter.is_empty (iter_vars layout) in
            if is_concrete then (
              Hashtbl.add cache ty layout;
              Result.ok layout)
            else
              let+ () = Poly.push_layout ty layout in
              Ok layout)
end

let size_of_int_ty = size_of_int_ty
let size_of_uint_ty = size_of_uint_ty
let size_of_literal_ty = size_of_literal_ty
let is_signed = is_signed

(* TODO: this is not really if we want to properly emulate different platforms,
   but this is good enough for now. See
   https://doc.rust-lang.org/reference/type-layout.html#r-layout.primitive.align *)
let align_of_literal_ty = size_of_literal_ty

type meta_kind = LenKind | VTableKind | NoneKind

let pp_meta_kind ft = function
  | LenKind -> Fmt.string ft "length"
  | VTableKind -> Fmt.string ft "vtable"
  | NoneKind -> Fmt.string ft "unit"

let rec dst_kind : Types.ty -> meta_kind = function
  | TSlice _ -> LenKind
  | TDynTrait _ -> VTableKind
  | TAdt adt when Crate.is_struct adt -> (
      match List.last_opt (Crate.as_struct adt) with
      | None -> NoneKind
      | Some last -> dst_kind Types.(last.field_ty))
  | TTraitType (tref, assoc_ty_id, args) ->
      dst_kind (resolve_trait_ty tref assoc_ty_id args)
  | TVar (Bound _) as ty ->
      L.failwith "dst_kind: unnormalized type: %a" pp_ty ty
  | _ -> NoneKind

(** Resolves the associated type [assoc_ty_id] of the trait reference [tref], if
    the trait reference is resolved to an implementation. *)
and resolve_trait_ty (tref : Types.trait_ref) assoc_ty_id args =
  match tref.kind with
  | TraitImpl timplref ->
      let impl = Crate.get_trait_impl timplref in
      let trait_assoc_ty = Types.AssocTypeId.Map.find assoc_ty_id impl.types in
      let trait_assoc_ty =
        Substitute.(
          apply_args_to_binder args
            st_substitute_visitor#visit_trait_assoc_ty_impl trait_assoc_ty)
      in
      trait_assoc_ty.value
  | BuiltinOrAuto (BuiltinPointee, _, _, _) -> (
      let pointee = List.hd tref.trait_decl_ref.binder_value.generics.types in
      match dst_kind pointee with
      | NoneKind -> TypesUtils.mk_unit_ty
      | LenKind -> TScalar (TInteger (Unsigned Usize))
      | VTableKind ->
          let adt = Crate.get_adt_lang_item RustcLangItemDynMetadata in
          let generics = TypesUtils.mk_generic_args_from_types [ pointee ] in
          TAdt { id = adt.def_id; generics; builtin = None })
  | _ -> L.failwith "resolve_trait_ty: can't resolve %a" Crate.pp_trait_ref tref

(** If this is a DST type with a slice tail, return the type of the slice's
    element. Errors otherwise. *)
let rec dst_slice_ty : Types.ty -> Types.ty = function
  | TSlice (sub_ty, _) -> sub_ty
  | TAdt adt when Crate.is_struct adt -> (
      match List.last_opt (Crate.as_struct adt) with
      | None -> L.failwith "dst_slice_ty: unexpected type"
      | Some last -> dst_slice_ty Types.(last.field_ty))
  | ty -> L.failwith "dst_slice_ty: unexpected type: %a" pp_ty ty

(** Returns the resulting type obtained when indexing into the given type. Only
    valid for arrays and slices. *)
let index_ty : Types.ty -> Types.ty = function
  | TSlice (ty, _) | TArray (ty, _, _) -> ty
  | ty -> L.failwith "slice_ty: unexpected type: %a" pp_ty ty

(** If this is a dynamically sized type (requiring a fat pointer). Requires the
    input type to be normalised. *)
let is_dst ty = dst_kind ty <> NoneKind

let[@inline] size_to_fit ~size ~align =
  Typed.ite
    (size %@ align ==@ Usize.(0s))
    size
    (size +!!@ align -!!@ (size %@ align))

(** Whether Charon chose all the offsets of the given layout; for now, we do not
    evaluate guaranteed or partially known layouts. *)
let layout_is_chosen (layout : Types.layout) =
  let rec discr_is_chosen : Types.discriminator -> bool = function
    | Known _ | Invalid -> true
    | Branch (offset, _, children, fallback) ->
        Option.is_some offset.chosen
        && List.for_all (fun (_, d) -> discr_is_chosen d) children
        && discr_is_chosen fallback
  in
  Option.for_all discr_is_chosen layout.discriminator
  && layout.variant_layouts
     |> List.for_all
        @@ Option.for_all (fun v ->
            v.Types.field_offsets
            |> List.for_all @@ fun (ofs : Types.offset_expr) ->
               Option.is_some ofs.chosen)

(** The alignment a field of alignment [align] has within a value of type [ty]:
    [repr(packed(n))] caps the alignment of every field to [n]. *)
let packed_align (ty : Types.ty) align =
  match ty with
  | TAdt { id; _ } -> (
      match (Crate.get_adt_raw id).layout with
      | [ (_triple, { repr = { align_modif = Some (Pack n); _ }; _ }) ] ->
          BV.min ~signed:false align (BV.usizeinz n)
      | _ -> align)
  | _ -> align

let mk ~size ~align ?(uninhabited = Typed.v_false)
    ?(fields : Fields_shape.t = Primitive) () =
  { size; align; uninhabited; fields }

let mk_concrete ~size ~align =
  mk ~size:(BV.usizei size) ~align:(BV.usizeinz align)

let not_impl_layout msg ty = not_impl "Can't compute layout: %s %a" msg pp_ty ty
let layout_warning msg ty = [%l.warn "Layout: %s@.Type: %a" msg pp_ty ty]

let rec layout_of (ty : Types.ty) : (t, 'e, 'f) Rustsymex.Result.t =
  let* ty = Poly.subst_ty ty in
  Session.get_or_compute_cached_layout ty @@ fun () ->
  match ty with
  (* Literals *)
  | TScalar ty ->
      let size = size_of_literal_ty ty in
      let align = align_of_literal_ty ty in
      ok (mk_concrete ~size ~align ())
  (* Fat pointers *)
  | (TRef (_, sub_ty, _) | TRawPtr (sub_ty, _)) when is_dst sub_ty ->
      let ptr_size = Crate.pointer_size () in
      ok
        (mk_concrete ~size:(ptr_size * 2) ~align:ptr_size
           ~fields:(Array { stride = BV.usizei ptr_size; is_ptr = true })
           ())
  (* Refs, pointers, function pointers *)
  | TRef (_, _, _) | TRawPtr (_, _) | TFnPtr _ ->
      let ptr_size = Crate.pointer_size () in
      ok (mk_concrete ~size:ptr_size ~align:ptr_size ())
  (* Dynamically sized types -- we assume they have a size of 0. In truth, these
     types should simply never be allocated directly, and instead can only be
     obtained hidden behind references; however we must be able to compute their
     layout, to get e.g. the offset of the tail in a DST struct. FIXME: Maybe we
     should mark the layout as a DST, and ensure a DST layout's size is never
     used for an allocation. *)
  | TSlice (sub_ty, _) ->
      let++ sub_layout = layout_of sub_ty in
      mk ~size:(BV.usizei 0) ~align:sub_layout.align
        ~fields:(Array { stride = sub_layout.size; is_ptr = false })
        ()
  (* Same as above, but here we have even less information ! *)
  | TDynTrait _ -> ok (mk_concrete ~size:0 ~align:1 ())
  (* ADTs (struct, enum, tuple, etc.) *)
  | TAdt adt -> (
      let adt = Crate.get_adt adt in
      match (adt.layout, adt.kind) with
      (* FIXME: Charon has surprising behaviour when translating layouts in
         polymorphic mode, meaning some types may have a layout while their
         fields don't. To avoid this, we *never* consider layouts of generic
         types, even if one is provided. This avoids inconsistent layouts. *)
      | [ (_triple, layout) ], _
        when (not (Config.get ()).polymorphic) && layout_is_chosen layout ->
          translate_layout adt.kind ty layout
      | _ :: _ :: _, _ -> L.failwith "multiple layouts for the same ADT"
      | _, Struct fields -> compute_arbitrary_layout ty (field_tys fields)
      | _, Union fields -> compute_union_layout ty (field_tys fields)
      | _, Enum variants -> compute_enum_layout ty variants
      | _, (Opaque | TDeclError _ | Alias _) -> not_impl_layout "unexpected" ty)
  (* Arrays *)
  | TArray (subty, size, _) ->
      let max_array_len sub_size =
        (* We calculate the max array size for a 32bit architecture, like Miri
           does. *)
        let isize_bits = 32 - 1 in
        BV.usize Z.(one lsl isize_bits) /@ Typed.cast sub_size
      in
      let len = BV.of_constant_expr size in
      let** sub_layout = layout_of subty in
      let++ () =
        assert_or_error
          (Typed.or_lazy
             (sub_layout.size ==@ Usize.(0s))
             (fun () -> len <=@ max_array_len sub_layout.size))
          (`InvalidLayout ty)
      in
      let size = len *!!@ sub_layout.size in
      mk ~size ~align:sub_layout.align
        ~fields:(Array { stride = sub_layout.size; is_ptr = false })
        ()
  (* Never -- zero sized type *)
  | TNever ->
      ok
        (mk_concrete ~size:0 ~align:1 ~uninhabited:Typed.v_true
           ~fields:Primitive ())
  (* Function definitions -- zero sized type *)
  | TFnDef _ -> ok (mk_concrete ~size:0 ~align:1 ~fields:Primitive ())
  (* Pattern types -- just their content, we assume they're primitives *)
  | TPattern (ty, _) -> layout_of ty
  (* Type variables : non-deterministically generate a layout *)
  | TVar (Free _) ->
      let* size = nondet (Typed.t_usize ()) in
      let* () = assume Usize.[ 0s <=$@ size; size <$@ 1024s ] in
      (* this is real non-determinism of the alignment; we don't do it because
         it creates quite expensive formulae that we want to avoid. We make the
         assumption the biggest possible alignment is 16, which is that of
         u128. *)
      (* let* align_shift = nondet (Typed.t_usize ()) in
       * let* () = assume Usize.[ 0s <=$@ align_shift; align_shift <=$@ 4s ] in
       * let align = Typed.cast (Usize.(1s) <<@ align_shift) in *)
      let align = Usize.(1s) in
      ok (mk ~size ~align ())
  | TVar (Bound _) ->
      L.failwith "escaping bound type variable found in layout_of"
  (* Others (unhandled for now) *)
  | TPtrMetadata _ -> not_impl_layout "pointer metadata" ty
  | TError _ -> not_impl_layout "error" ty
  | TTraitType (tref, assoc_ty_id, args) ->
      layout_of (resolve_trait_ty tref assoc_ty_id args)

and translate_discriminator : Types.discriminator -> Fields_shape.discriminator
    = function
  | Known v -> Known v
  | Invalid -> Invalid
  | Branch (offset, int_ty, children, fallback) ->
      let offset = Option.get ~msg:"discriminator: unknown ofs" offset.chosen in
      let offset = BV.usizei offset in
      let children =
        List.map
          (fun ((from_, to_), discr) ->
            (BV.of_scalar from_, BV.of_scalar to_, translate_discriminator discr))
          children
      in
      let tag_ty = lit_of_int_ty int_ty in
      let fallback = translate_discriminator fallback in
      Branch { offset; tag_ty; children; fallback }

(** Evaluates an inhabitedness predicate; predicates that can't be decided are
    considered inhabited. *)
and is_inhabited : Types.inhabited_predicate -> _ = function
  | InhabitedPredicateTrue -> ok Typed.v_true
  | InhabitedPredicateFalse -> ok Typed.v_false
  | InhabitedPredicateAnd preds ->
      Result.fold_list preds ~init:Typed.v_true ~f:(fun acc pred ->
          let++ pred = is_inhabited pred in
          acc &&@ pred)
  | InhabitedPredicateOr preds ->
      Result.fold_list preds ~init:Typed.v_false ~f:(fun acc pred ->
          let++ pred = is_inhabited pred in
          acc ||@ pred)
  | InhabitedPredicateConstIsZero { kind = CInteger v; _ } ->
      ok (BV.of_scalar v ==@ Usize.(0s))
  | InhabitedPredicateConstIsZero c ->
      not_impl "inhabited predicate: %a == 0" Crate.pp_constant_expr c
  | InhabitedPredicateGenericType ty ->
      let++ layout = layout_of ty in
      Typed.not layout.uninhabited

and translate_layout adt_kind ty (layout : Types.layout) =
  let size = compute_size ty layout.size in
  let align = compute_align ty layout.align in
  let** inhabited = is_inhabited layout.inhabited in
  let uninhabited = Typed.not inhabited in
  let discriminator = Option.map translate_discriminator layout.discriminator in
  let variant_layouts =
    List.map
      (fun v_opt : (Fields_shape.tagger * Fields_shape.t) ->
        match (v_opt : Types.variant_layout option) with
        | None -> (None, Primitive)
        | Some v ->
            let ofs =
              v.field_offsets
              |> List.map (fun (ofs : Types.offset_expr) ->
                  Option.get ~msg:"layout: unknown ofs" ofs.chosen |> BV.usizei)
              |> Array.of_list
            in
            let tagger =
              match v.tagger with
              | [] -> None
              | [ (ofs, value) ] -> Some (BV.usizei ofs, BV.of_scalar value)
              | _ :: _ :: _ -> L.failwith "unsupported: >1 tagger values"
            in
            (tagger, Arbitrary ofs))
      layout.variant_layouts
  in
  let is_enum = [%matches? Enum _] adt_kind in
  let fields : Fields_shape.t =
    match (discriminator, variant_layouts) with
    (* no variants/invalid, so this is uninhabited; we can use primitive *)
    | _, [] | Some Invalid, _ -> Primitive
    (* struct/union: there is only one inhabited variant *)
    | Some (Known v), _ when not is_enum ->
        snd (Types.VariantId.nth variant_layouts v)
    (* enums *)
    | Some ((Known _ | Branch _) as d), _ ->
        Enum (d, Array.of_list variant_layouts)
    (* we didn't translate a discriminator; we hope it's a struct-like! *)
    | None, [ (_, v) ] -> v
    | None, _ -> L.failwith "no discriminator and >1 variant layouts?"
  in
  let layout = mk ~size ~align ~uninhabited ~fields () in
  [%l.trace "Translated layout for %a:@.%a" pp_ty ty pp layout];
  ok layout

and compute_size ty size =
  match size.chosen with
  | Some (SizeExprConstant { kind = CInteger s; _ }) -> BV.usize (z_of_scalar s)
  | _ ->
      layout_warning "Inferred size=0" ty;
      BV.usizei 0

and compute_align ty align =
  match align.chosen with
  | Some (SizeExprConstant { kind = CInteger a; _ }) ->
      BV.usizenz (z_of_scalar a)
  | _ ->
      layout_warning "Inferred align=1" ty;
      BV.usizeinz 1

and compute_arbitrary_layout ?fst_size ?fst_align ty members =
  (* Note: here we manually calculate a layout, à la [repr(C)]. We should avoid doing this,
     and make it clearer when we do. *)
  (* Calculates the offsets, size and alignment for a tuple-like type with fields of
     the given types. Also returns a symbolic boolean to assert this calculation did
     not overflow. *)
  let rec aux offsets curr_size curr_align overflowed uninhabited = function
    | [] -> ok (List.rev offsets, curr_size, curr_align, overflowed, uninhabited)
    | ty :: rest ->
        let** layout = layout_of ty in
        let offset = size_to_fit ~size:curr_size ~align:layout.align in
        let new_size, ovf = offset +$?@ layout.size in
        let new_align = BV.max ~signed:false layout.align curr_align in
        aux (offset :: offsets) new_size new_align (ovf ||@ overflowed)
          (uninhabited ||@ layout.uninhabited)
          rest
  in
  let fst_size = Option.value fst_size ~default:(BV.usizei 0) in
  let fst_align = Option.value fst_align ~default:(BV.usizeinz 1) in
  let** offsets, size, align, overflowed, uninhabited =
    aux [] fst_size fst_align Typed.v_false Typed.v_false members
  in
  let++ () = assert_or_error (Typed.not overflowed) (`InvalidLayout ty) in
  let size = size_to_fit ~size ~align in
  let layout =
    mk ~size ~align ~uninhabited ~fields:(Arbitrary (Array.of_list offsets)) ()
  in
  Fmt.kstr layout_warning "Computed an arbitrary layout:@.%a" pp layout ty;
  layout

and compute_enum_layout ty (variants : Types.variant list) =
  match variants with
  (* no variants: uninhabited ZST *)
  | [] ->
      layout_warning "Computed an enum layout (ZST)" ty;
      ok
        (mk_concrete ~size:0 ~align:1 ~uninhabited:Typed.v_true
           ~fields:Primitive ())
  (* N variants: we assume a tagged variant *)
  | _ :: _ ->
      let tags =
        List.map
          (fun (v : Types.variant) -> BV.of_scalar v.discriminant)
          variants
      in
      let tag_ty =
        match variants with
        | [] -> Values.TInteger (Signed I32) (* Shouldn't matter *)
        | v :: _ -> lit_of_scalar v.discriminant
      in
      let discriminator : Fields_shape.discriminator =
        (* best effort: we assume direct encoding *)
        let children =
          List.map
            (fun (v : Types.variant) ->
              let tag = Types.VariantId.nth tags v.id in
              (tag, tag, Fields_shape.Known v.id))
            variants
        in
        let offset = Usize.(0s) in
        Branch { offset; tag_ty; children; fallback = Invalid }
      in
      let** tag = layout_of (TScalar tag_ty) in
      let++ size, align, variants, uninhabited =
        Result.fold_list variants
          ~init:(Usize.(0s), Usize.(1s), [], Typed.v_true)
          ~f:(fun (size, align, variants, uninhabited) v ->
            let** l =
              compute_arbitrary_layout ty ~fst_size:tag.size
                ~fst_align:tag.align (field_tys v.fields)
            in
            let+ tagger =
              if%sat l.uninhabited then return None
              else return (Some (BV.usizei 0, Types.VariantId.nth tags v.id))
            in
            Ok
              ( BV.max ~signed:false size l.size,
                BV.max ~signed:false align l.align,
                (tagger, l.fields) :: variants,
                uninhabited &&@ l.uninhabited ))
      in
      let variants = List.rev variants in
      let layout =
        mk ~size ~align ~uninhabited
          ~fields:(Enum (discriminator, Array.of_list variants))
          ()
      in
      Fmt.kstr layout_warning "Computed an enum layout:@.%a" pp layout ty;
      layout

and compute_union_layout ty members =
  layout_warning "Computed a union layout" ty;
  let++ size, align =
    Result.fold_list members
      ~init:(Usize.(0s), Usize.(1s))
      ~f:(fun (size, align) member_ty ->
        let++ member = layout_of member_ty in
        ( BV.max ~signed:false size member.size,
          BV.max ~signed:false align member.align ))
  in
  let fields = Array.make (List.length members) (BV.usizei 0) in
  mk ~size ~align ~fields:(Arbitrary fields) ()

(** Normalise a type, by substituting any generics with the current generic
    environment, and resolving the trait type if needed. *)
let normalise (ty : Types.ty) =
  let* ty = Poly.subst_ty ty in
  match ty with
  | TTraitType (tref, name, args) -> return (resolve_trait_ty tref name args)
  | _ -> return ty

let[@inline] size_of ty =
  let++ { size; _ } = layout_of ty in
  (Typed.cast size :> Typed.([> T.sint ] t))

let[@inline] align_of ty =
  let++ { align; _ } = layout_of ty in
  (Typed.cast align :> Typed.([> T.nonzero ] t))

(** The byte offset of [field] within the vtable of the [dyn Trait] predicate
    [dyn_pred]. *)
let vtable_field_offset (dyn_pred : Types.dyn_predicate)
    (field : Types.v_table_field) =
  match Crate.dyn_vtable_ref dyn_pred with
  | None ->
      (* Obol doesn't declare VTable structs, using instead the rustc hardcoded
         layout. *)
      let idx =
        match field with
        | VTableSize -> 1
        | VTableAlign -> 2
        | _ -> L.failwith "vtable_field_offset: unexpected field"
      in
      ok (BV.usizei (idx * Crate.pointer_size ()))
  | Some vt_ref ->
      let idx = Crate.vtable_field_index vt_ref field in
      let++ { fields; _ } = layout_of (TAdt vt_ref) in
      Fields_shape.offset_of idx fields

(** Whether the given type is a 1ZST: it must have size 0 and alignment 1. *)
let[@inline] is_1zst ty =
  let++ layout = layout_of ty in
  layout.size ==@ Usize.(0s) &&@ (layout.align ==@ Usize.(1s))

(** Whether the given type is a ZST: it must have size 0. *)
let[@inline] is_zst ty =
  let++ layout = layout_of ty in
  layout.size ==@ Usize.(0s)

(** If the given type is an enum with a single variant, return the variant's ID.
    Otherwise, return None. Fails if the type is not an enum. *)
let enum_single_variant (ty : Types.ty) =
  let++ layout = layout_of ty in
  match layout.fields with
  | Enum (Known v, _) -> Some v
  | Enum _ -> None
  | _ -> L.failwith "enum_single_variant: not an enum type: %a" pp_ty ty

let min_value_z : Types.integer_type -> Z.t = function
  | Unsigned _ -> Z.zero
  | Signed Isize -> Z.neg (Z.shift_left Z.one ((8 * Crate.pointer_size ()) - 1))
  | Signed I128 -> Z.neg (Z.shift_left Z.one 127)
  | Signed I64 -> Z.neg (Z.shift_left Z.one 63)
  | Signed I32 -> Z.neg (Z.shift_left Z.one 31)
  | Signed I16 -> Z.neg (Z.shift_left Z.one 15)
  | Signed I8 -> Z.neg (Z.shift_left Z.one 7)

let max_value_z : Types.integer_type -> Z.t = function
  | Unsigned U128 -> Z.pred (Z.shift_left Z.one 128)
  | Unsigned U64 -> Z.pred (Z.shift_left Z.one 64)
  | Unsigned U32 -> Z.pred (Z.shift_left Z.one 32)
  | Unsigned U16 -> Z.pred (Z.shift_left Z.one 16)
  | Unsigned U8 -> Z.pred (Z.shift_left Z.one 8)
  | Unsigned Usize -> Z.pred (Z.shift_left Z.one (8 * Crate.pointer_size ()))
  | Signed I128 -> Z.pred (Z.shift_left Z.one 127)
  | Signed I64 -> Z.pred (Z.shift_left Z.one 63)
  | Signed I32 -> Z.pred (Z.shift_left Z.one 31)
  | Signed I16 -> Z.pred (Z.shift_left Z.one 15)
  | Signed I8 -> Z.pred (Z.shift_left Z.one 7)
  | Signed Isize ->
      Z.pred (Z.shift_left Z.one ((8 * Crate.pointer_size ()) - 1))

(** Whether the type implements [Freeze], i.e. has no interior mutability. *)
let rec is_freeze : Types.ty -> bool = function
  | TAdt adt -> (
      let decl = Crate.get_adt adt in
      match decl.marker_traits with
      | Some { is_freeze; _ } -> is_freeze
      (* Charon only knows the marker traits of monomorphic types *)
      | None -> (
          if adt_is_unsafe_cell adt then false
          else
            match decl.kind with
            | Struct fs | Union fs -> List.for_all is_freeze (field_tys fs)
            | Enum vs ->
                Iter.for_all is_freeze
                @@ Iter.flat_map_l (fun (v : Types.variant) ->
                    field_tys v.fields)
                @@ Iter.of_list vs
            | _ -> true))
  | TArray (ty, _, _) | TSlice (ty, _) -> is_freeze ty
  | _ -> true

(** [is_abi_compatible ty1 ty2] is true if a function expecting an argument of
    type [ty1] can be called with an argument of type [ty2].

    The full specification is available at:
    https://doc.rust-lang.org/nightly/std/primitive.fn.html#abi-compatibility *)
let rec is_abi_compatible (ty1 : Types.ty) (ty2 : Types.ty) =
  let is_ptr_like : Types.ty -> bool = function
    | TRef _ | TRawPtr _ -> true
    | TAdt { builtin = Some TBox; _ } -> true
    | _ -> false
  in
  let is_repr_transparent (adt : Types.type_decl_ref) =
    [%matches? [ (_triple, { repr = { transparent = true; _ }; _ }) ]]
      (Crate.get_adt_raw adt.id).layout
  in
  let rec find_non_zst_field = function
    | [] -> Result.ok None
    | ty :: rest ->
        let** is_zst = is_1zst ty in
        if%sat is_zst then find_non_zst_field rest else ok (Some ty)
  in
  let as_transparent (adt : Types.type_decl_ref) =
    match (Crate.get_adt adt).kind with
    | Struct fields -> find_non_zst_field (field_tys fields)
    | Enum [ v ] -> find_non_zst_field (field_tys v.fields)
    | _ -> ok None
  in
  match (ty1, ty2) with
  (* Hack: &dyn is always compatible *)
  | TDynTrait _, _
  | ( (TRef (_, TDynTrait _, _) | TRawPtr (TDynTrait _, _)),
      (TRef (_, _, _) | TRawPtr (_, _)) ) ->
      ok Typed.v_true
  (* Refs and raw pointers are ABI-compatible if they have the same metadata
     type *)
  | (TRef (_, ty1, _) | TRawPtr (ty1, _)), (TRef (_, ty2, _) | TRawPtr (ty2, _))
    ->
      ok (Typed.of_bool (dst_kind ty1 = dst_kind ty2))
  | TScalar (TInteger (Unsigned uint1)), TScalar (TInteger (Unsigned uint2)) ->
      ok (Typed.of_bool (size_of_uint_ty uint1 = size_of_uint_ty uint2))
  | TScalar (TInteger (Signed int1)), TScalar (TInteger (Signed int2)) ->
      ok (Typed.of_bool (size_of_int_ty int1 = size_of_int_ty int2))
  | TScalar (TInteger (Unsigned U32)), TScalar TChar
  | TScalar TChar, TScalar (TInteger (Unsigned U32)) ->
      ok Typed.v_true
  (* patterns: recurse *)
  | TPattern (inner1, _), ty2 -> is_abi_compatible inner1 ty2
  | ty1, TPattern (inner2, _) -> is_abi_compatible ty1 inner2
  (* Function pointers are compatible if they have the same ABI-string *)
  | TFnPtr sig1, TFnPtr sig2 ->
      ok
        (Typed.of_bool
           (Types.equal_abi sig1.binder_value.abi sig2.binder_value.abi))
  (* We keep this later down to avoid the check for everything *)
  | ty1, ty2 when is_ptr_like ty1 && is_ptr_like ty2 -> ok Typed.v_true
  (* transparent ADTs: recurse *)
  | TAdt adt1, _ when is_repr_transparent adt1 -> (
      let** ty1 = as_transparent adt1 in
      match ty1 with
      | None -> is_1zst ty2
      | Some ty1 -> is_abi_compatible ty1 ty2)
  | _, TAdt adt2 when is_repr_transparent adt2 -> (
      let** ty2 = as_transparent adt2 in
      match ty2 with
      | None -> is_1zst ty1
      | Some ty2 -> is_abi_compatible ty1 ty2)
  | ty1, ty2 when Types.equal_ty ty1 ty2 -> ok Typed.v_true
  | _ ->
      let** ty1_1zst = is_1zst ty1 in
      let++ ty2_1zst = is_1zst ty2 in
      (* 1ZSTs are exclusively compatible with themselves; otherwise type
         equality! *)
      ty1_1zst &&@ ty2_1zst

(** Returns the path through an ADT to the pointer that is the target of an
    unsizing operation from [from_ty] to [to_ty]; [None] is returned if no path
    was found.

    The path is found by recursively exploring the field whose type differs
    between the source and the target structure, ignoring 1ZSTs, until a pointer
    is found. *)
let unsize_path ~(from_ty : Types.ty) ~(to_ty : Types.ty) :
    (int list option, _, _) Result.t =
  let ( let*** ) x f = bind (function Some x -> f x | None -> ok None) x in
  let rec find_coerced_field idx from_tys to_tys =
    match (from_tys, to_tys) with
    | [], [] -> ok None
    | [], _ | _, [] -> L.failwith "unsize_path: mismatched types"
    | from_ty :: from_rest, to_ty :: to_rest ->
        if Types.equal_ty from_ty to_ty then
          find_coerced_field (idx + 1) from_rest to_rest
        else
          let** is_1zst = is_1zst from_ty in
          if%sat is_1zst then find_coerced_field (idx + 1) from_rest to_rest
          else ok (Some (idx, from_ty, to_ty))
  in
  let rec aux acc from_ty to_ty =
    match (from_ty, to_ty) with
    | (Types.TRawPtr _ | TRef _), _ -> ok (Some (List.rev acc))
    | TPattern (from_ty, _), _ -> aux acc from_ty to_ty
    | _, Types.TPattern (to_ty, _) -> aux acc from_ty to_ty
    | TAdt from_adt, TAdt to_adt
      when Crate.is_struct from_adt && Crate.is_struct to_adt ->
        let*** idx, from_ty, to_ty =
          find_coerced_field 0
            (Crate.as_struct_tys from_adt)
            (Crate.as_struct_tys to_adt)
        in
        aux (idx :: acc) from_ty to_ty
    | _ -> ok None
  in
  aux [] from_ty to_ty
