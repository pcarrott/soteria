open Soteria.Symex.Make (Tiny_solver.Z3_solver)
open Syntax
open Soteria.Tiny_values.Typed
open Infix

(* ============================================================================
   Test Processes
   ============================================================================ *)

let bounded =
  let* x = nondet t_int in
  let* () = assume [ int 5 <@ x; x <@ int 7 ] in
  return [ x ]

let negative =
  let* x = nondet t_int in
  let* () = assume [ int (-5) <@ x; x <@ int (-3) ] in
  return [ x ]

let shared =
  let* x = nondet t_int in
  let* y = nondet t_int in
  let* () = assume [ x +@ y ==@ int 10; x ==@ int 3 ] in
  return [ x; y ]

let unconstrained =
  let* x = nondet t_int in
  let* y = nondet t_int in
  let* () = assume [ x ==@ int 3 ] in
  return [ x; y ]

(* ============================================================================
   Helper
   ============================================================================ *)

let show_value model v =
  match kind v with
  | Var x ->
      Option.map (Fmt.to_to_string Expr.pp) (Svalue.Var.Map.find_opt x model)
  | _ -> Alcotest.fail "Expected a variable"

let models process =
  ListLabels.map (run ~mode:UX process) ~f:(fun (vs, pcs) ->
      Option.map (fun model -> List.map (show_value model) vs) (model ~pcs))

(* ============================================================================
   Tests
   ============================================================================ *)

let check_model name expected process () =
  Alcotest.(check (list (option (list (option string)))))
    name [ Some expected ] (models process)

let unsat_pcs_test () =
  let pcs = [ Expr.of_value v_false ] in
  Alcotest.(check bool) "model" true (Option.is_none (model ~pcs))

(* ============================================================================
   Test Runner
   ============================================================================ *)

let () =
  Alcotest.run "Model"
    [
      ( "Model",
        [
          Alcotest.test_case "model_of_bounded" `Quick
            (check_model "model" [ Some "6" ] bounded);
          Alcotest.test_case "model_of_negative" `Quick
            (check_model "model" [ Some "-4" ] negative);
          Alcotest.test_case "model_is_shared" `Quick
            (check_model "model" [ Some "3"; Some "7" ] shared);
          Alcotest.test_case "model_omits_unconstrained" `Quick
            (check_model "model" [ Some "3"; None ] unconstrained);
          Alcotest.test_case "model_of_unsat_pcs" `Quick unsat_pcs_test;
        ] );
    ]
