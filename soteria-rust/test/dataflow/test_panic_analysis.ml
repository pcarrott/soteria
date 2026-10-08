open Soteria_rust_lib
open Soteria.Soteria_std
open Syntaxes.FunctionWrap
open List

(** The crate of [panic_examples.rs], parsed once with the default frontend and
    shared across the test cases. *)
let crate = lazy (Frontend.parse_ullbc (`File "panic_examples.rs"))

(** [all_paths_panic fn_name] resolves the function named [fn_name] in the
    example crate and runs {!Analyses.Panic_analysis.all_paths_panic} on it. *)
let all_paths_panic fn_name =
  let crate = Lazy.force crate in
  let@ () = Crate.with_crate crate in
  let last_segment name = Fmt.to_to_string Crate.pp_name [ List.last name ] in
  let decl =
    Charon.Types.FunDeclId.Map.values crate.fun_decls
    |> List.find (fun (d : Charon.UllbcAst.fun_decl) ->
        String.equal (last_segment d.item_meta.name) fn_name)
  in
  Analyses.Dataflow.Never.is_never decl.def_id

let check fn_name expected () =
  Alcotest.(check bool)
    (Fmt.str "all_paths_panic %s" fn_name)
    expected (all_paths_panic fn_name)

let cases =
  [
    (* Functions do have a terminating case *)
    ("ok", false);
    ("some_panic", false);
    ("call_some_panic_or_panic", false);
    ("call_some_panic_or_return", false);
    ("call_some_panic_or_never", false);
    ("call_some_panic", false);
    ("count_down", false);
    ("loop_with_break", false);
    ("even", false);
    ("odd", false);
    ("calls_returning_fn", false);
    ("panic_helper_or_return", false);
    ("maybe_panic_in_loop", false);
    ("partial_match_panic", false);
    (* Functions that are "never". *)
    ("do_panic", true);
    ("calls_panic", true);
    ("match_panic", true);
    ("ping", true);
    ("pong", true);
    ("never", true);
    ("spin", true);
    ("calls_exit", true);
    ("panic_and_continue", true);
    ("slice_index_order_fail", true);
    ("assert_false", true);
    ("debug_assert_false", true);
    ("propagate", true);
  ]

let () =
  let cases =
    List.map
      (fun (name, expected) ->
        let test_name =
          Fmt.str "%s is %snever" name (if expected then "" else "not ")
        in
        Alcotest.test_case test_name `Quick (check name expected))
      cases
  in
  Alcotest.run "panic_analysis" [ ("is_never", cases) ]
