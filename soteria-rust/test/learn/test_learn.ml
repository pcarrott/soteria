open Soteria_rust_lib
module Typed = Svalue.Typed
module Expr = Typed.Expr
open Charon

let var i ty = Typed.mk_var (Soteria.Symex.Var.of_int i) ty
let usize i = Typed.BitVec.usizei i
let usize_var i = var i (Typed.t_usize ())

let adt : Types.type_decl_ref =
  {
    id = Types.TypeDeclId.of_int 0;
    generics = TypesUtils.empty_generic_args;
    builtin = None;
  }

let enum variant fields =
  Typed.Adt.mk_enum adt (Types.VariantId.of_int variant) fields

(* Learns [pattern := value], and returns the pattern instantiated with the
   learned substitution, if it could be learned. *)
let learn pattern value =
  let pattern = Expr.of_value pattern in
  Expr.Subst.learn Expr.Subst.empty pattern value
  |> Option.map (fun subst ->
      let v, _ =
        Expr.Subst.apply
          ~missing_var:(fun _ _ -> Alcotest.fail "variable not learned")
          subst pattern
      in
      Fmt.str "%a" Typed.ppa v)

let check name pattern value =
  Alcotest.(check (option string))
    name
    (Some (Fmt.str "%a" Typed.ppa value))
    (learn pattern value)

let test_enum () =
  check "fields of the same variant"
    (enum 1 [ usize_var 1; usize_var 2 ])
    (enum 1 [ usize 3; usize 4 ])

let test_enum_variant_mismatch () =
  Alcotest.(check (option string))
    "a different variant cannot be learned" None
    (learn (enum 0 [ usize_var 1 ]) (enum 1 [ usize 3 ]))

let thin_ptr ~loc ~ofs ~size =
  Typed.Ptr.mk_ptr_t ~loc ~ofs ~size ~align:(Typed.BitVec.usizeinz 8) ~tag:None

let test_ptr () =
  check "location, offset and size of a full pointer"
    (Typed.Ptr.of_ptr_t
       (thin_ptr
          ~loc:(var 1 (Typed.t_loc ()))
          ~ofs:(usize_var 2) ~size:(usize_var 3)))
    (Typed.Ptr.of_ptr_t
       (thin_ptr ~loc:(Typed.Ptr.loc_of_int 5) ~ofs:(usize 16) ~size:(usize 24)))

let test_array () =
  let usize_ty : Types.ty = TScalar (TInteger (Unsigned Usize)) in
  check "elements of an array"
    (Typed.Adt.mk_array usize_ty (Iarray.of_list [ usize_var 1; usize_var 2 ]))
    (Typed.Adt.mk_array usize_ty (Iarray.of_list [ usize 3; usize 4 ]))

let () =
  Alcotest.run "Learn"
    [
      ( "learn",
        [
          Alcotest.test_case "enum" `Quick test_enum;
          Alcotest.test_case "enum variant mismatch" `Quick
            test_enum_variant_mismatch;
          Alcotest.test_case "pointer" `Quick test_ptr;
          Alcotest.test_case "array" `Quick test_array;
        ] );
    ]
