module Charon = struct
  include Charon

  module Types = struct
    include Charon.Types

    let map_ty (f : type_var_id -> type_var_id) =
      let visitor =
        object
          inherit [_] map_ty

          method! visit_TVar _ =
            function Free id -> TVar (Free (f id)) | Bound _ as v -> TVar v
        end
      in
      visitor#visit_ty ()

    let iter_ty (ty : ty) f =
      let visitor =
        object
          inherit [_] iter_ty
          method! visit_TVar _ = function Free id -> f id | Bound _ -> ()
        end
      in
      visitor#visit_ty () ty

    let mentions id (ty : ty) =
      let exception Found in
      try
        iter_ty ty (fun id' -> if TypeVarId.equal_id id id' then raise Found);
        false
      with Found -> true

    (* Regions and trait references do not distinguish the values of a type *)
    let erase =
      let visitor =
        object (self)
          inherit [_] map_ty
          method! visit_region _ _ = RErased

          method! visit_generic_args env generics =
            {
              generics with
              regions = [];
              types = List.map (self#visit_ty env) generics.types;
              trait_refs = [];
            }
        end
      in
      visitor#visit_ty ()

    (* Numbers type variables from 0, in order of first occurrence. *)
    let numbering () : type_var_id -> type_var_id =
      let seen = ref [] in
      fun id ->
        match List.assoc_opt id !seen with
        | Some n -> n
        | None ->
            let n = TypeVarId.of_int (List.length !seen) in
            seen := (id, n) :: !seen;
            n

    (* The renaming of the type variables of [ty] into those of its canonical
       form. *)
    let to_canonical ty =
      let number = numbering () in
      iter_ty (erase ty) (fun id -> ignore (number id));
      number

    (* The renaming of the type variables of the canonical form of [ty] back
       into those of [ty]. Other type variables are unrelated to those of [ty]:
       they are moved out of their way. *)
    let of_canonical ty =
      let number = numbering () in
      let bindings = ref [] in
      iter_ty (erase ty) (fun id -> bindings := (number id, id) :: !bindings);
      fun id ->
        match List.assoc_opt id !bindings with
        | Some id -> id
        | None -> TypeVarId.of_int (0x10000 + TypeVarId.to_int id)

    (* The canonical form of [ty] is unique up to a one-to-one renaming of type
       variables. *)
    let canonical ty = map_ty (to_canonical ty) (erase ty)
  end

  (** Prints Charon items as Rust source, resolving their declarations in the
      crate of the environment *)
  module PrintRust = struct
    type env = {
      fmt_env : PrintFmt.fmt_env;
      in_crate : bool;
          (** Whether the items of the crate are referred to from within it,
              through [crate::], rather than through the crate's name *)
      infer_ty_vars : bool;
          (** Whether type variables are left to be inferred, as [_], rather
              than printed by their names in [fmt_env] *)
    }

    let mk_env ?(in_crate = false) ?(infer_ty_vars = false) fmt_env =
      { fmt_env; in_crate; infer_ty_vars }

    let pp_string = Format.pp_print_string

    let int_ty_name : Values.int_ty -> string = function
      | Isize -> "isize"
      | I8 -> "i8"
      | I16 -> "i16"
      | I32 -> "i32"
      | I64 -> "i64"
      | I128 -> "i128"

    let u_int_ty_name : Values.u_int_ty -> string = function
      | Usize -> "usize"
      | U8 -> "u8"
      | U16 -> "u16"
      | U32 -> "u32"
      | U64 -> "u64"
      | U128 -> "u128"

    let float_ty_name : Types.float_type -> string = function
      | F16 -> "f16"
      | F32 -> "f32"
      | F64 -> "f64"
      | F128 -> "f128"

    let scalar_name : Types.scalar_type -> string = function
      | TInteger (Signed ty) -> int_ty_name ty
      | TInteger (Unsigned ty) -> u_int_ty_name ty
      | TFloat ty -> float_ty_name ty
      | TBool -> "bool"
      | TChar -> "char"

    let pp_path (env : env) ~(is_local : bool) (fmt : Format.formatter)
        (name : Types.name) : unit =
      let pp_elem fmt : Types.path_elem -> unit = function
        | PeIdent (ident, _) -> pp_string fmt ident
        | elem -> PrintFmt.pp_path_elem env.fmt_env fmt elem
      in
      match name with
      | PeIdent _ :: name when is_local && env.in_crate ->
          Format.fprintf fmt "crate::%a"
            (PrintFmt.pp_sep_list "::" pp_elem)
            name
      | name -> PrintFmt.pp_sep_list "::" pp_elem fmt name

    let rec pp_ty (env : env) (fmt : Format.formatter) (ty : Types.ty) : unit =
      match ty with
      | TScalar ty -> pp_string fmt (scalar_name ty)
      | TAdt { builtin = Some TTuple; generics = { types; _ }; _ } ->
          let comma = if List.length types = 1 then "," else "" in
          Format.fprintf fmt "(%a%s)"
            (PrintFmt.pp_sep_list ", " (pp_ty env))
            types comma
      | TAdt { builtin = Some TStr; _ } -> pp_string fmt "str"
      | TAdt adt -> pp_adt env ~turbofish:false fmt adt
      | TRef (_, ty, RShared) -> Format.fprintf fmt "&%a" (pp_ty env) ty
      | TRef (_, ty, RMut) -> Format.fprintf fmt "&mut %a" (pp_ty env) ty
      | TRawPtr (ty, RShared) -> Format.fprintf fmt "*const %a" (pp_ty env) ty
      | TRawPtr (ty, RMut) -> Format.fprintf fmt "*mut %a" (pp_ty env) ty
      | TArray (ty, len, _) ->
          Format.fprintf fmt "[%a; %a]" (pp_ty env) ty
            (PrintFmt.pp_constant_expr env.fmt_env)
            len
      | TSlice (ty, _) -> Format.fprintf fmt "[%a]" (pp_ty env) ty
      | TVar _ when env.infer_ty_vars -> pp_string fmt "_"
      (* The remaining types are printed in Charon's syntax *)
      | _ -> PrintFmt.pp_ty env.fmt_env fmt ty

    and pp_adt (env : env) ~(turbofish : bool) (fmt : Format.formatter)
        ({ id; generics; builtin } : Types.type_decl_ref) : unit =
      let { item_meta = { name; is_local; _ }; _ } : Types.type_decl =
        Types.TypeDeclId.Map.find id env.fmt_env.crate.type_decls
      in
      (match builtin with
      | Some TBox -> pp_string fmt "Box"
      | _ -> pp_path env ~is_local fmt name);
      pp_generics env ~turbofish fmt generics

    and pp_generics (env : env) ~(turbofish : bool) (fmt : Format.formatter)
        ({ types; const_generics; _ } : Types.generic_args) : unit =
      let pp_arg fmt = function
        | `Ty ty -> pp_ty env fmt ty
        | `Const c -> PrintFmt.pp_constant_expr env.fmt_env fmt c
      in
      match
        List.map (fun ty -> `Ty ty) types
        @ List.map (fun c -> `Const c) const_generics
      with
      | [] -> ()
      | args ->
          let turbofish = if turbofish then "::" else "" in
          Format.fprintf fmt "%s<%a>" turbofish
            (PrintFmt.pp_sep_list ", " pp_arg)
            args

    let pp_impl (env : env) (fmt : Format.formatter) : Types.impl_elem -> unit =
      function
      | ImplElemTy { binder_value = TAdt ({ builtin = None; _ } as adt); _ } ->
          pp_adt env ~turbofish:true fmt adt
      | ImplElemTy { binder_value = ty; _ } ->
          Format.fprintf fmt "<%a>" (pp_ty env) ty
      | ImplElemTrait impl_id -> (
          let crate = env.fmt_env.crate in
          let { id; generics } : Types.trait_decl_ref =
            (UllbcAst.TraitImplId.Map.find impl_id crate.trait_impls).impl_trait
          in
          let { item_meta = { name; is_local; _ }; _ } : UllbcAst.trait_decl =
            Types.TraitDeclId.Map.find id crate.trait_decls
          in
          match generics.types with
          | self :: types ->
              Format.fprintf fmt "<%a as %a%a>" (pp_ty env) self
                (pp_path env ~is_local) name
                (pp_generics env ~turbofish:false)
                { generics with types }
          | [] ->
              failwith
                ("Trait impl has no self type: "
                ^ PrintFmt.pp_to_string (fun fmt ->
                    PrintFmt.pp_name env.fmt_env fmt name)))

    let pp_fun (env : env) (fmt : Format.formatter)
        ({ item_meta = { name; is_local; _ }; _ } : UllbcAst.fun_decl) : unit =
      match List.rev name with
      | PeIdent (method_, _) :: PeImpl impl :: _ ->
          Format.fprintf fmt "%a::%s" (pp_impl env) impl method_
      | _ -> pp_path env ~is_local fmt name
  end
end
