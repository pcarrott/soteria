open Charon
open Soteria_rust_lib

module Typed = struct
  include Svalue.Typed

  let rename_tvars f v = Expr.of_value (Adt.map_poly f (type_ v))
end

module Value = struct
  type t = Typed.T.any Typed.t
  type syn = Typed.Expr.t

  let pp fmt : t -> unit = Typed.pp Typed.T.pp_any fmt
  let pp_syn fmt : syn -> unit = Typed.Expr.pp fmt
  let to_syn : t -> syn = Typed.Expr.of_value
  let iter_vars (syn : syn) = Typed.Svalue.iter_vars syn
  let subst : (syn -> t) -> syn -> t = Typed.Expr.subst
  let of_ptr : [> Typed.T.sptr_f ] Typed.t -> t = Typed.as_any
  let as_ptr : t -> [< Typed.T.sptr_f ] Typed.t = Typed.cast_ptr_f
  let sem_eq : t -> t -> Typed.sbool Typed.t = Typed.sem_eq
  let learn_eq (syn : syn) (ret : t) = Rustsymex.Consumer.learn_eq syn ret
  let rename_tvars f : syn -> syn = Typed.rename_tvars f

  let nondet (ty : Types.ty) : t Rustsymex.t =
    let open Rustsymex.Syntax in
    let* res, _ =
      Layout.SM.run_with_state ~state:None (Value_codec.nondet_valid ty)
    in
    match res with
    | Soteria.Soteria_std.Compo_res.Ok ret -> Rustsymex.return ret
    | _ -> failwith "Expected Ok in nondet"
end

(** A witness is either a primitive value or a function call preceded by the
    witnesses of its arguments and followed by a sequence of drops and memory
    accesses. The witness is safe code, and each statement reads every value
    that it moves or drops, as the analysis does. Succeeding memory accesses
    only occur when the function call outputs a reference.

    The variables of a witness are symbolic variables, which are printed as Rust
    variables: producing a summary renames them to fresh ones, so they are
    always distinct. A primitive value is named by its own variable, and a call
    by a variable [ret] for its output, whereas [result] is the variable holding
    the value that the witness describes: either [ret] or the variable of a
    reference argument. *)
type 'v expr = Value of Types.ty * 'v | Call of 'v call

and 'v call = {
  fun_decl : UllbcAst.fun_decl;
  args : 'v arg list;
  ret : 'v;
  stmts : 'v stmt list;
  result : 'v;
}

and 'v arg = Types.ref_kind option * 'v expr

and 'v stmt =
  | Drop of 'v
  | Keep of 'v * 'v
      (** [Keep (keep, var)] moves [var] to [keep], which is only dropped at the
          end of the witness *)
  | Read of 'v * 'v option
      (** Reads through a shared reference: the value is copied to the given
          variable, or the reference is retagged when the value is not [Copy] *)
  | Write of 'v * 'v expr * 'v option
      (** Replaces the value behind a reference; the old value is dropped, or
          moved to the given variable *)

type t = Value.t expr
type syn = Value.syn expr

let result = function Value (_, v) -> v | Call { result; _ } -> result

let rec map_values t f = function
  | Value (ty, v) -> Value (t ty, f v)
  | Call { fun_decl; args; ret; stmts; result } ->
      let args = List.map (map_values_arg t f) args in
      let stmts = List.map (map_values_stmt t f) stmts in
      Call { fun_decl; args; ret = f ret; stmts; result = f result }

and map_values_arg t f (ref_kind, value) = (ref_kind, map_values t f value)

and map_values_stmt t f = function
  | Drop var -> Drop (f var)
  | Keep (keep, var) -> Keep (f keep, f var)
  | Read (var, copy) -> Read (f var, Option.map f copy)
  | Write (var, value, old) ->
      Write (f var, map_values t f value, Option.map f old)

let to_syn : t -> syn = map_values Fun.id Value.to_syn
let subst : (Value.syn -> Value.t) -> syn -> t = map_values Fun.id
let rename_tvars f : syn -> syn = map_values (Types.map_ty f) Fun.id

(** {2 The witness monad}

    A state monad over [Rustsymex], like [State.SM], whose state holds the
    witness of the current call, with its arguments and statements in reverse
    order. *)

type state = Value.t call option

let empty : state = None

(* A fresh variable that only names a value in the printed witness *)
let fresh_name () = Value.nondet (TScalar (TInteger (Unsigned Usize)))

module WM = struct
  type result = t

  include
    Soteria.Sym_states.State_monad.Make
      (Rustsymex)
      (struct
        type t = state
      end)

  open Syntax

  let update f : unit t = map_state (Option.map f)

  (* The output of the call is named by a dedicated variable, as its value may
     not be a variable: a struct is a tuple of variables, and a zero-sized value
     has none. *)
  let ret_var =
    let* state = get_state () in
    match state with
    | Some { ret; _ } -> return ret
    | None -> lift (fresh_name ())

  let with_call fun_decl : unit t =
    let* ret = ret_var in
    set_state (Some { fun_decl; args = []; ret; stmts = []; result = ret })

  let add_arg arg : unit t = update (fun c -> { c with args = arg :: c.args })

  let add_stmt stmt : unit t =
    update (fun c -> { c with stmts = stmt :: c.stmts })

  let set_result result : unit t = update (fun c -> { c with result })

  let run_with_witness ~witness (x : 'a t) : ('a * result) Rustsymex.t =
    let open Rustsymex.Syntax in
    let+ res, state = run_with_state ~state:witness x in
    match state with
    | Some c ->
        (res, Call { c with args = List.rev c.args; stmts = List.rev c.stmts })
    | None -> failwith "Expected a call in the witness"
end

(** {2 Rust syntax} *)

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

(* The items of the analysed crate are referred to from its root *)
let pp_path ~is_local ft (name : Types.name) =
  let pp_elem ft : Types.path_elem -> unit = function
    | PeIdent (ident, _) -> Fmt.string ft ident
    | elem -> Crate.mk_pp PrintFmt.pp_path_elem ft elem
  in
  match name with
  | PeIdent _ :: name when is_local ->
      Fmt.pf ft "crate::%a" (Fmt.list ~sep:(Fmt.any "::") pp_elem) name
  | name -> Fmt.list ~sep:(Fmt.any "::") pp_elem ft name

let rec pp_ty ft (ty : Types.ty) =
  match ty with
  | TScalar ty -> Fmt.string ft (scalar_name ty)
  | TAdt { builtin = Some TTuple; generics = { types; _ }; _ } ->
      let comma = if List.length types = 1 then "," else "" in
      Fmt.pf ft "(%a%s)" (Fmt.list ~sep:Fmt.comma pp_ty) types comma
  | TAdt { builtin = Some TStr; _ } -> Fmt.string ft "str"
  | TAdt adt -> pp_adt ~turbofish:false ft adt
  | TRef (_, ty, RShared) -> Fmt.pf ft "&%a" pp_ty ty
  | TRef (_, ty, RMut) -> Fmt.pf ft "&mut %a" pp_ty ty
  | TRawPtr (ty, RShared) -> Fmt.pf ft "*const %a" pp_ty ty
  | TRawPtr (ty, RMut) -> Fmt.pf ft "*mut %a" pp_ty ty
  | TArray (ty, len, _) ->
      Fmt.pf ft "[%a; %a]" pp_ty ty Crate.pp_constant_expr len
  | TSlice (ty, _) -> Fmt.pf ft "[%a]" pp_ty ty
  (* The types of a witness are inferred from its opaque values *)
  | TVar _ -> Fmt.string ft "_"
  | _ -> Crate.mk_pp PrintFmt.pp_ty ft ty

and pp_adt ~turbofish ft ({ id; generics; builtin } : Types.type_decl_ref) =
  let { item_meta = { name; is_local; _ }; _ } : Types.type_decl =
    Crate.get_adt_raw id
  in
  (match builtin with
  | Some TBox -> Fmt.string ft "Box"
  | _ -> pp_path ~is_local ft name);
  pp_generics ~turbofish ft generics

and pp_generics ~turbofish ft
    ({ types; const_generics; _ } : Types.generic_args) =
  let pp_arg ft = function
    | `Ty ty -> pp_ty ft ty
    | `Const c -> Crate.pp_constant_expr ft c
  in
  match
    List.map (fun ty -> `Ty ty) types
    @ List.map (fun c -> `Const c) const_generics
  with
  | [] -> ()
  | args ->
      let turbofish = if turbofish then "::" else "" in
      Fmt.pf ft "%s<%a>" turbofish (Fmt.list ~sep:Fmt.comma pp_arg) args

let pp_impl ft : Types.impl_elem -> unit = function
  | ImplElemTy { binder_value = TAdt ({ builtin = None; _ } as adt); _ } ->
      pp_adt ~turbofish:true ft adt
  | ImplElemTy { binder_value = ty; _ } -> Fmt.pf ft "<%a>" pp_ty ty
  | ImplElemTrait impl_id -> (
      let { id; generics } : Types.trait_decl_ref =
        (Crate.get_trait_impl_raw impl_id).impl_trait
      in
      let { item_meta = { name; is_local; _ }; _ } : UllbcAst.trait_decl =
        Crate.get_trait_decl_raw id
      in
      match generics.types with
      | self :: types ->
          Fmt.pf ft "<%a as %a%a>" pp_ty self (pp_path ~is_local) name
            (pp_generics ~turbofish:false)
            { generics with types }
      | [] -> Fmt.failwith "Trait impl %a has no self type" Crate.pp_name name)

let pp_fun ft ({ item_meta = { name; is_local; _ }; _ } : UllbcAst.fun_decl) =
  match List.rev name with
  | PeIdent (method_, _) :: PeImpl impl :: _ ->
      Fmt.pf ft "%a::%s" pp_impl impl method_
  | _ -> pp_path ~is_local ft name

(** {2 Printing}

    A witness is printed as a sequence of statements, which bind the arguments
    of each call before running it. *)

let pp_var ft (v : Value.syn) =
  match Typed.Svalue.kind v with
  | Var var -> Fmt.pf ft "v%d" (Typed.Svalue.Var.to_int var)
  | _ -> Fmt.failwith "Expected a variable in the witness: %a" Value.pp_syn v

(* The variables that the witness [w] borrows mutably *)
let rec mut_vars = function
  | Value _ -> []
  | Call { args; stmts; _ } ->
      List.concat_map
        (fun (ref_kind, value) ->
          let vars = mut_vars value in
          if ref_kind = Some Types.RMut then result value :: vars else vars)
        args
      @ List.concat_map
          (function Write (_, value, _) -> mut_vars value | _ -> [])
          stmts

let stmt_var = function
  | Drop var | Keep (_, var) | Read (var, _) | Write (var, _, _) -> var

(* Prints the statements running the witness [w]; [top] holds if nothing refers
   to the result of [w] *)
let rec pp_stmts ~pp_value ~muts ~top ft w =
  let module Sv = Typed.Svalue in
  let pp_let ft var =
    let mut_ = if List.exists (Sv.equal var) muts then "mut " else "" in
    Fmt.pf ft "let %s%a = " mut_ pp_var var
  in
  match w with
  | Value (ty, v) -> Fmt.pf ft "@,%a%a;" pp_let v pp_value (ty, v)
  | Call { fun_decl; args; ret; stmts; result } ->
      List.iter (pp_arg_stmts ~pp_value ~muts ft) args;
      let is_bound =
        ((not top) && Sv.equal ret result)
        || List.exists (fun stmt -> Sv.equal ret (stmt_var stmt)) stmts
      in
      Fmt.pf ft "@,";
      if is_bound then pp_let ft ret;
      Fmt.pf ft "%a(%a);" pp_fun fun_decl
        (Fmt.list ~sep:(Fmt.any ", ") (pp_arg ~pp_value))
        args;
      List.iter (pp_stmt ~pp_value ~muts ft) stmts

(* Prints the statements binding an argument, unless it is a primitive value
   passed directly *)
and pp_arg_stmts ~pp_value ~muts ft = function
  | None, Value _ -> ()
  | _, value -> pp_stmts ~pp_value ~muts ~top:false ft value

and pp_arg ~pp_value ft (ref_kind, value) =
  match (ref_kind, value) with
  | None, Value (ty, v) -> pp_value ft (ty, v)
  | Some Types.RShared, _ -> Fmt.pf ft "&%a" pp_var (result value)
  | Some RMut, _ -> Fmt.pf ft "&mut %a" pp_var (result value)
  | None, _ -> pp_var ft (result value)

and pp_stmt ~pp_value ~muts ft = function
  | Drop var -> Fmt.pf ft "@,std::mem::drop(%a);" pp_var var
  | Keep (keep, var) -> Fmt.pf ft "@,let _%a = %a;" pp_var keep pp_var var
  | Read (var, Some copy) -> Fmt.pf ft "@,let _%a = *%a;" pp_var copy pp_var var
  | Read (var, None) -> Fmt.pf ft "@,std::hint::black_box(%a);" pp_var var
  | Write (var, value, old) -> (
      pp_arg_stmts ~pp_value ~muts ft (None, value);
      match old with
      | None ->
          Fmt.pf ft "@,std::mem::drop(std::mem::replace(%a, %a));" pp_var var
            (pp_arg ~pp_value) (None, value)
      | Some old ->
          Fmt.pf ft "@,let _%a = std::mem::replace(%a, %a);" pp_var old pp_var
            var (pp_arg ~pp_value) (None, value))

let pp_syn ft (w : syn) =
  let pp_value ft (_, v) = Value.pp_syn ft v in
  Fmt.pf ft "@[<v>%a@]" (pp_stmts ~pp_value ~muts:(mut_vars w) ~top:true) w

(** {2 Concrete programs} *)

type program = string

(* A type variable is instantiated by a local struct of this name, with the size
   of its layout *)
let pp_ty_var ft id = Fmt.pf ft "T%d" (Types.TypeVarId.to_int id)

(* The type variables of the opaque values of the witness [w] *)
let rec ty_vars = function
  | Value (TVar (Free id), _) -> [ id ]
  | Value _ -> []
  | Call { args; stmts; _ } ->
      List.concat_map (fun (_, value) -> ty_vars value) args
      @ List.concat_map
          (function Write (_, value, _) -> ty_vars value | _ -> [])
          stmts

(** A test running the statements of the witness [w], with the values in a model
    of the path condition [pcs], and type variables of the sizes in [layouts] in
    that model. *)
let program ~pcs ?(layouts = []) (w : t) : program =
  let w = to_syn w in
  let ids = List.sort_uniq Stdlib.compare (ty_vars w) in
  (* Type variables are zero-sized, or else of size one, when [pcs] allows it *)
  let pcs =
    let open Typed.Infix in
    let open Typed.Syntax in
    ListLabels.fold_left ids ~init:pcs ~f:(fun pcs id ->
        match List.assoc_opt id layouts with
        | None -> pcs
        | Some size -> (
            let with_size n =
              let pcs = Typed.Expr.of_value (size ==@ n) :: pcs in
              Option.map (fun _ -> pcs) (Rustsymex.MonoSymex.model ~pcs)
            in
            match with_size Usize.(0s) with
            | Some pcs -> pcs
            | None -> Option.value ~default:pcs (with_size Usize.(1s))))
  in
  let model = Rustsymex.MonoSymex.model ~pcs in
  let module Sv = Typed.Svalue in
  (* Variables that are not in the model are not constrained by [pcs], so they
     can take any value *)
  let concrete model (v : Value.syn) =
    let missing_var var ty =
      let ty = Typed.untype_type ty in
      Typed.type_
      @@
      match (Sv.Var.Map.find_opt var model, ty) with
      | Some c, _ -> c
      | None, TBool -> Sv.Bool.v_false
      | None, TBitVector n -> Sv.BitVec.mk n Z.zero
      | None, TFloat fp -> Sv.Float.mk_bits fp Z.zero
      | None, _ -> Sv.mk_var var ty
    in
    Typed.Expr.of_value
    @@ fst (Typed.Expr.Subst.apply ~missing_var Typed.Expr.Subst.empty v)
  in
  let size id =
    let open Syntaxes.Option in
    let* model = model in
    let* size = List.assoc_opt id layouts in
    Sv.BitVec.to_z (concrete model (Typed.Expr.of_value size))
  in
  let pp_value ft ((ty : Types.ty), (v : Value.syn)) =
    let c = Option.map (fun model -> concrete model v) model in
    let bits = Option.bind Sv.BitVec.to_z c in
    match (ty, bits) with
    | TVar (Free id), _ -> (
        match size id with
        | Some n when Z.gt n Z.zero ->
            Fmt.pf ft "%a([0; %a])" pp_ty_var id Z.pp_print n
        | _ -> pp_ty_var ft id)
    | TScalar TBool, Some bits -> Fmt.bool ft (not (Z.equal bits Z.zero))
    | TScalar TChar, Some bits -> Fmt.pf ft "'\\u{%s}'" (Z.format "%x" bits)
    | TScalar (TInteger (Signed ty)), Some bits ->
        let n = Sv.BitVec.bv_to_z true (Sv.size_of v.node.ty) bits in
        Fmt.pf ft "%a%s" Z.pp_print n (int_ty_name ty)
    | TScalar (TInteger (Unsigned ty)), Some bits ->
        Fmt.pf ft "%a%s" Z.pp_print bits (u_int_ty_name ty)
    | TScalar (TFloat ty), _ -> (
        match
          Option.bind Sv.BitVec.to_z (Option.bind Sv.Float.to_bits_opt c)
        with
        | Some bits ->
            Fmt.pf ft "%s::from_bits(%s)" (float_ty_name ty)
              (Z.format "%#x" bits)
        | None -> Value.pp_syn ft v)
    | _ -> Value.pp_syn ft v
  in
  let buf = Buffer.create 512 in
  let buf_ft = Format.formatter_of_buffer buf in
  Format.pp_set_geometry buf_ft ~max_indent:9998 ~margin:9999;
  let pp_struct ft id =
    match size id with
    | Some n when Z.gt n Z.zero ->
        Fmt.pf ft "@,struct %a([u8; %a]);" pp_ty_var id Z.pp_print n
    | _ -> Fmt.pf ft "@,struct %a;" pp_ty_var id
  in
  Fmt.pf buf_ft "@[<v>#[test]@,@[<v 4>fn witness() {%a%a@]@,}@]@?"
    (Fmt.list ~sep:Fmt.nop pp_struct)
    ids
    (pp_stmts ~pp_value ~muts:(mut_vars w) ~top:true)
    w;
  Buffer.contents buf

let pp_program : program Fmt.t = Fmt.string
