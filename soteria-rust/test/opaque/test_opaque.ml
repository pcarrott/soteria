open Soteria.Soteria_std
open Soteria_rust_lib
module Typed = Svalue.Typed
module Symex = Rustsymex
module Opaque = State.Opaque.Block
module Bi = Soteria.Sym_states.Bi_abd.Make (Symex) (Opaque)
open Symex.Syntax

let run process = Symex.run ~mode:UX process
let int i = Typed.as_any (Typed.BitVec.u32i i)
let expr i = Typed.Expr.of_value (int i)
let nondet () = Symex.map Typed.as_any (Symex.nondet (Typed.t_int 32))

let drop_outcomes process =
  run process
  |> List.map (fun (res, _) ->
      match res with
      | Compo_res.Ok st -> Fmt.str "ok %a" (Fmt.Dump.option Opaque.pp) st
      | Missing [ [ fix ] ] -> Fmt.str "miss %a" Opaque.pp_syn fix
      | Missing _ -> "miss (unexpected shape)"
      | Error _ -> "error")
  |> List.sort String.compare

let check name expected process =
  Alcotest.(check (list string)) name expected (drop_outcomes process)

let drop v st =
  let+ res, st = Opaque.drop v st in
  Compo_res.map (fun () -> st) res

let test_drop_owned () =
  check "moved to dropped"
    [ "ok Some Opaque(, 0x00000001)" ]
    (drop (int 1) (Some ([ int 1 ], [])))

let test_drop_miss () =
  check "fix owns the dropped value"
    [ "miss Opaque(0x00000001, )" ]
    (drop (int 1) None)

let test_drop_not_syntactic () =
  (* [x] and [1] are distinct values syntactically, but they may be equal, so we
     must find both outcomes. *)
  check "symbolic comparison"
    [ "miss Opaque(V|1|, )"; "ok Some Opaque(, V|1|)" ]
    (let* x = nondet () in
     drop x (Some ([ int 1 ], [])))

let test_drop_equal_implied () =
  (* A syntactically different value that must be equal has to match. *)
  check "implied equality"
    [ "ok Some Opaque(, V|1|)" ]
    (let* x = nondet () in
     let* () = Symex.assume [ Typed.sem_eq x (int 1) ] in
     drop x (Some ([ int 1 ], [])))

let test_drop_multiset () =
  check "only one instance is removed"
    [ "ok Some Opaque(0x00000001, 0x00000001)" ]
    (drop (int 1) (Some ([ int 1; int 1 ], [])))

let produce syn st = Symex.Producer.run_identity (Opaque.produce syn st)

let test_produce () =
  let process =
    let+ st = produce ([ expr 1 ], [ expr 2 ]) (Some ([ int 3 ], [])) in
    Compo_res.Ok st
  in
  check "adds to both sides"
    [ "ok Some Opaque(0x000000010x00000003, 0x00000002)" ]
    process

let test_produce_duplicate () =
  (* Values are distinct resources: owning one twice is a contradiction *)
  let process =
    let+ st = produce ([ expr 1 ], []) (Some ([], [ int 1 ])) in
    Compo_res.Ok st
  in
  check "vanishes on duplicates" [] process

let consume syn st =
  let+ res =
    Symex.Consumer.run ~subst:Typed.Expr.Subst.empty (Opaque.consume syn st)
  in
  Compo_res.map fst res

let test_consume () =
  check "removes from both sides" [ "ok None" ]
    (consume ([ expr 1 ], [ expr 2 ]) (Some ([ int 1 ], [ int 2 ])));
  check "owned and dropped are separate"
    [ "miss Opaque(0x00000001, )" ]
    (consume ([ expr 1 ], []) (Some ([], [ int 1 ])));
  check "missing dropped gets a dropped fix"
    [ "miss Opaque(, 0x00000002)" ]
    (consume ([], [ expr 2 ]) None)

let test_round_trip () =
  check "to_syn, then produce, then consume is empty" [ "ok None" ]
    (let st = ([ int 1; int 2 ], [ int 3 ]) in
     let syn = List.hd (Opaque.to_syn st) in
     let* st = produce syn None in
     consume syn st)

let test_bi_abduction () =
  (* The fix from a [drop] miss has to be enough for the retry to succeed. *)
  let results =
    run (Bi.wrap (Opaque.drop (int 1)) None)
    |> List.map (fun ((res, st), _) ->
        match res with
        | Compo_res.Ok () ->
            let ~pre, ~post = Bi.to_spec st in
            Fmt.str "pre %a post %a"
              (Fmt.Dump.list Opaque.pp_syn)
              pre
              (Fmt.Dump.list Opaque.pp_syn)
              post
        | Missing _ | Error _ -> "failed")
  in
  Alcotest.(check (list string))
    "abduces ownership"
    [ "pre [Opaque(0x00000001, )] post [Opaque(, 0x00000001)]" ]
    results

let () =
  Alcotest.run "Opaque"
    [
      ( "drop",
        [
          Alcotest.test_case "owned" `Quick test_drop_owned;
          Alcotest.test_case "miss" `Quick test_drop_miss;
          Alcotest.test_case "symbolic" `Quick test_drop_not_syntactic;
          Alcotest.test_case "implied equality" `Quick test_drop_equal_implied;
          Alcotest.test_case "multiset" `Quick test_drop_multiset;
          Alcotest.test_case "bi-abduction" `Quick test_bi_abduction;
        ] );
      ( "logic",
        [
          Alcotest.test_case "produce" `Quick test_produce;
          Alcotest.test_case "duplicate" `Quick test_produce_duplicate;
          Alcotest.test_case "consume" `Quick test_consume;
          Alcotest.test_case "round trip" `Quick test_round_trip;
        ] );
    ]
