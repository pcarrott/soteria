open Soteria_rust_lib
module Typed = Svalue.Typed
open Rustsymex.Syntax
open Charon

let usize i = Typed.BitVec.usizei i
let ptr_to ty : Types.ty = TRawPtr (TScalar (TInteger ty), RShared)

(* Whether a valid symbolic value of type [ty] can be equal to [v] *)
let can_equal ty v =
  let process =
    let* res, _ =
      Layout.SM.run_with_state ~state:None (Value_codec.nondet_valid ty)
    in
    match res with
    | Soteria.Soteria_std.Compo_res.Ok nondet ->
        Rustsymex.assume [ Typed.sem_eq nondet (Typed.as_any v) ]
    | _ -> Alcotest.fail "Expected a symbolic value"
  in
  Layout.Session.with_layout_cache @@ fun () ->
  not (List.is_empty (Rustsymex.run ~mode:UX process))

(* The allocation of a pointer is not given by its type: [Vec<i32>] stores its
   buffer as a pointer to [u8] *)
let test_other_allocation () =
  let ptr =
    Typed.Ptr.mk_ptr_t ~loc:(Typed.Ptr.loc_of_int 5) ~ofs:(usize 0)
      ~size:(usize 4) ~align:(Typed.BitVec.usizeinz 4) ~tag:None
  in
  Alcotest.(check bool)
    "pointer to u8 into an i32 allocation" true
    (can_equal (ptr_to (Unsigned U8)) (Typed.Ptr.of_ptr_t ptr))

(* An empty [Vec<i32>] holds a dangling pointer, which has no allocation *)
let test_dangling () =
  Alcotest.(check bool)
    "dangling pointer to i32" true
    (can_equal (ptr_to (Signed I32)) (Typed.Ptr.of_address_f (usize 4)))

let () =
  Alcotest.run "Nondet"
    [
      ( "pointer",
        [
          Alcotest.test_case "other allocation" `Quick test_other_allocation;
          Alcotest.test_case "dangling" `Quick test_dangling;
        ] );
    ]
