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

  (* Whether all the variables of [v] can be learned when it is matched against
     another value, as when checking implications between summaries, where the
     variables of [known] have already been learned. This is the learnability
     test of consumption, applied to a placeholder value. *)
  let is_learnable ~known (v : t) =
    let learn_var subst (x, ty) =
      let var = Typed.mk_var x ty in
      match subst with
      | Some subst -> Typed.Expr.Subst.learn subst (Typed.Expr.of_value var) var
      | None -> None
    in
    let known =
      Iter.of_list known
      |> Iter.flat_map Typed.iter_vars
      |> Iter.fold learn_var (Some Typed.Expr.Subst.empty)
    in
    let syn = to_syn v in
    let placeholder =
      Typed.mk_var (Soteria.Symex.Var.of_int 0) (Typed.Expr.ty syn)
    in
    match known with
    | Some subst ->
        Option.is_some (Typed.Expr.Subst.learn subst syn placeholder)
    | None -> false

  (* Assumes the validity invariants of [ty] for [v], including the constraints
     on the allocation information of its pointers, which only describe how
     pointers are represented *)
  let assume_valid (ty : Types.ty) (v : t) : unit Rustsymex.t =
    let open Rustsymex.Syntax in
    let* res, _ =
      Layout.SM.run_with_state ~state:None
        (Value_codec.validity ~alloc_info:true ty v (fun c _err ->
             let open Layout.SM.Syntax in
             let+ () = Layout.SM.assume [ c ] in
             Soteria.Soteria_std.Compo_res.Ok ()))
    in
    match res with
    | Soteria.Soteria_std.Compo_res.Ok () -> Rustsymex.return ()
    | _ -> failwith "Expected Ok in assume_valid"
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
  fun_name : string;  (** The path of the function, printed in Rust *)
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
  | Call { fun_name; args; ret; stmts; result } ->
      let args = List.map (map_values_arg t f) args in
      let stmts = List.map (map_values_stmt t f) stmts in
      Call { fun_name; args; ret = f ret; stmts; result = f result }

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
    (* The witness is a test within the analysed crate, whose types are inferred
       from its values *)
    let env =
      PrintRust.mk_env ~in_crate:true ~infer_ty_vars:true (Crate.as_fmt_env ())
    in
    let fun_name = Fmt.str "@[<h>%a@]" (PrintRust.pp_fun env) fun_decl in
    set_state (Some { fun_name; args = []; ret; stmts = []; result = ret })

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

(** {2 Concrete programs} *)

(** A witness whose values are paired with their instances in a model, and the
    sizes of the type variables in that model *)
type program = {
  witness : (Value.syn * Value.syn) expr;
  sizes : (Types.TypeVarId.id * Z.t) list;
}

(* The type variables of the opaque values of the witness [w] *)
let rec ty_vars = function
  | Value (TVar (Free id), _) -> [ id ]
  | Value _ -> []
  | Call { args; stmts; _ } ->
      List.concat_map (fun (_, value) -> ty_vars value) args
      @ List.concat_map
          (function Write (_, value, _) -> ty_vars value | _ -> [])
          stmts

(** Instantiates the witness [w] with a model of the path condition [pcs], and
    type variables of the sizes in [layouts] in that model. *)
let program ~pcs ~layouts (w : t) : program =
  let w = to_syn w in
  let ids = List.sort_uniq Types.TypeVarId.compare_id (ty_vars w) in
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
  let model =
    match Rustsymex.MonoSymex.model ~pcs with
    | Some model -> model
    | None -> failwith "The path condition of a witness is unsatisfiable"
  in
  let module Sv = Typed.Svalue in
  (* Variables that are not in the model are not constrained by [pcs], so they
     can take any value *)
  let concrete (v : Value.syn) =
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
  (* A type variable without a layout has an unconstrained size *)
  let size id =
    match List.assoc_opt id layouts with
    | Some size ->
        Option.get (Sv.BitVec.to_z (concrete (Typed.Expr.of_value size)))
    | None -> Z.zero
  in
  {
    witness = map_values Fun.id (fun v -> (v, concrete v)) w;
    sizes = List.map (fun id -> (id, size id)) ids;
  }

(** {2 Printing}

    A witness is printed as a sequence of statements, which bind the arguments
    of each call before running it. *)

let pp_var ft (v : Value.syn) =
  match Typed.Svalue.kind v with
  | Var var -> Fmt.pf ft "v%d" (Typed.Svalue.Var.to_int var)
  | _ -> Fmt.failwith "Expected a variable in the witness: %a" Value.pp_syn v

(* The variables that the witness [w] borrows mutably *)
let rec mut_vars ~var = function
  | Value _ -> []
  | Call { args; stmts; _ } ->
      List.concat_map
        (fun (ref_kind, value) ->
          let vars = mut_vars ~var value in
          if ref_kind = Some Types.RMut then var (result value) :: vars
          else vars)
        args
      @ List.concat_map
          (function Write (_, value, _) -> mut_vars ~var value | _ -> [])
          stmts

let stmt_var = function
  | Drop var | Keep (_, var) | Read (var, _) | Write (var, _, _) -> var

(* Prints the statements running the witness [w], whose variables are given by
   [var]; [top] holds if nothing refers to the result of [w] *)
let rec pp_stmts ~var ~pp_value ~muts ~top ft w =
  let module Sv = Typed.Svalue in
  let pp_var ft v = pp_var ft (var v) in
  let pp_let ft v =
    let mut_ = if List.exists (Sv.equal (var v)) muts then "mut " else "" in
    Fmt.pf ft "let %s%a = " mut_ pp_var v
  in
  match w with
  | Value (ty, v) -> Fmt.pf ft "@,%a%a;" pp_let v pp_value (ty, v)
  | Call { fun_name; args; ret; stmts; result } ->
      List.iter (pp_arg_stmts ~var ~pp_value ~muts ft) args;
      let is_bound =
        ((not top) && Sv.equal (var ret) (var result))
        || List.exists
             (fun stmt -> Sv.equal (var ret) (var (stmt_var stmt)))
             stmts
      in
      Fmt.pf ft "@,";
      if is_bound then pp_let ft ret;
      Fmt.pf ft "%s(%a);" fun_name
        (Fmt.list ~sep:(Fmt.any ", ") (pp_arg ~var ~pp_value))
        args;
      List.iter (pp_stmt ~var ~pp_value ~muts ft) stmts

(* Prints the statements binding an argument, unless it is a primitive value
   passed directly *)
and pp_arg_stmts ~var ~pp_value ~muts ft = function
  | None, Value _ -> ()
  | _, value -> pp_stmts ~var ~pp_value ~muts ~top:false ft value

and pp_arg ~var ~pp_value ft (ref_kind, value) =
  let pp_var ft v = pp_var ft (var v) in
  match (ref_kind, value) with
  | None, Value (ty, v) -> pp_value ft (ty, v)
  | Some Types.RShared, _ -> Fmt.pf ft "&%a" pp_var (result value)
  | Some RMut, _ -> Fmt.pf ft "&mut %a" pp_var (result value)
  | None, _ -> pp_var ft (result value)

and pp_stmt ~var ~pp_value ~muts ft stmt =
  let pp_var ft v = pp_var ft (var v) in
  match stmt with
  | Drop v -> Fmt.pf ft "@,std::mem::drop(%a);" pp_var v
  | Keep (keep, v) -> Fmt.pf ft "@,let _%a = %a;" pp_var keep pp_var v
  | Read (v, Some copy) -> Fmt.pf ft "@,let _%a = *%a;" pp_var copy pp_var v
  | Read (v, None) -> Fmt.pf ft "@,std::hint::black_box(%a);" pp_var v
  | Write (v, value, old) -> (
      pp_arg_stmts ~var ~pp_value ~muts ft (None, value);
      match old with
      | None ->
          Fmt.pf ft "@,std::mem::drop(std::mem::replace(%a, %a));" pp_var v
            (pp_arg ~var ~pp_value) (None, value)
      | Some old ->
          Fmt.pf ft "@,let _%a = std::mem::replace(%a, %a);" pp_var old pp_var v
            (pp_arg ~var ~pp_value) (None, value))

let pp_syn ft (w : syn) =
  let var = Fun.id in
  let pp_value ft (_, v) = Value.pp_syn ft v in
  Fmt.pf ft "@[<v>%a@]"
    (pp_stmts ~var ~pp_value ~muts:(mut_vars ~var w) ~top:true)
    w

(* A type variable is instantiated by a local struct of this name, with the size
   of its layout *)
let pp_ty_var ft id = Fmt.pf ft "T%d" (Types.TypeVarId.to_int id)

(** Prints the program [p] as a test running the statements of its witness *)
let pp_program ft ({ witness; sizes } : program) =
  let module Sv = Typed.Svalue in
  let size id = List.assoc id sizes in
  (* The primitive values of a witness are scalars or opaque values, and the
     model instantiates scalars with constants *)
  let pp_value ft ((ty : Types.ty), ((_, c) : Value.syn * Value.syn)) =
    let to_z = function
      | Some bits -> bits
      | None ->
          Fmt.failwith "Expected a constant in the witness: %a" Value.pp_syn c
    in
    let bits () = to_z (Sv.BitVec.to_z c) in
    match ty with
    | TVar (Free id) ->
        let n = size id in
        if Z.gt n Z.zero then Fmt.pf ft "%a([0; %a])" pp_ty_var id Z.pp_print n
        else pp_ty_var ft id
    | TScalar TBool -> Fmt.bool ft (not (Z.equal (bits ()) Z.zero))
    | TScalar TChar -> Fmt.pf ft "'\\u{%s}'" (Z.format "%x" (bits ()))
    | TScalar (TInteger (Signed ty)) ->
        let n = Sv.BitVec.bv_to_z true (Sv.size_of c.node.ty) (bits ()) in
        Fmt.pf ft "%a%s" Z.pp_print n (PrintRust.int_ty_name ty)
    | TScalar (TInteger (Unsigned ty)) ->
        Fmt.pf ft "%a%s" Z.pp_print (bits ()) (PrintRust.u_int_ty_name ty)
    | TScalar (TFloat ty) ->
        let bits = to_z (Option.bind Sv.BitVec.to_z (Sv.Float.to_bits_opt c)) in
        Fmt.pf ft "%s::from_bits(%s)"
          (PrintRust.float_ty_name ty)
          (Z.format "%#x" bits)
    | _ ->
        Fmt.failwith "Expected a scalar or opaque value in the witness: %a"
          Value.pp_syn c
  in
  let buf = Buffer.create 512 in
  let buf_ft = Format.formatter_of_buffer buf in
  Format.pp_set_geometry buf_ft ~max_indent:9998 ~margin:9999;
  let pp_struct ft (id, n) =
    if Z.gt n Z.zero then
      Fmt.pf ft "@,struct %a([u8; %a]);" pp_ty_var id Z.pp_print n
    else Fmt.pf ft "@,struct %a;" pp_ty_var id
  in
  let var = fst in
  Fmt.pf buf_ft "@[<v>#[test]@,@[<v 4>fn witness() {%a%a@]@,}@]@?"
    (Fmt.list ~sep:Fmt.nop pp_struct)
    sizes
    (pp_stmts ~var ~pp_value ~muts:(mut_vars ~var witness) ~top:true)
    witness;
  Fmt.string ft (Buffer.contents buf)
