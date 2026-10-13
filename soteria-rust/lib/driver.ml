open Soteria.Terminal.Diagnostic
open Soteria.Logs.Printers
open Syntaxes.FunctionWrap
open Analyses.Util

let wrap_step name f =
  Fmt.epr "%a...@?" (pp_style `Bold) name;
  try
    let time = Unix.gettimeofday () in
    let res = f () in
    let time = Unix.gettimeofday () -. time in
    Fmt.epr " done in %a@." pp_time time;
    res
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    Fmt.epr " errored@.";
    Printexc.raise_with_backtrace e bt

let with_exn_and_config mode config f =
  try
    Config.set_and_lock_global mode config;
    let outcome = f () in
    Analyses.Outcome.exit outcome
  with
  | Frontend.FrontendError e -> fatal ~name:"Frontend" ~code:3 e
  | Frontend.CompilationError (info, msg) ->
      print_diagnostic_simple ~severity:Error "Compilation error";
      Fmt.pr "@.%s@.%a@.@." msg Unimplemented.pp
        (Unimplemented.make
           ~tip:
             ( "You can try cleaning plugins and rebuilding them",
               Some "soteria-rust build-plugins [compilation flags]" )
           ~issue:388
           ("Compilation failed while " ^ info));
      Analyses.Outcome.exit Error
  | Exn.Config_error err ->
      fatal ~name:"Config" ~code:Cmdliner.Cmd.Exit.cli_error err
  | Analyses.Biab.Uncaught.Uncaught_exception (where, e) ->
      let bt = Printexc.get_backtrace () in
      let phase =
        match where with
        | Generating_summary fn -> Fmt.str "generating summary for %s" fn
        | Analysing_summary fn -> Fmt.str "analysing summary for %s" fn
      in
      Fmt.epr "%a while %s:@\n  %s@.@?" pp_err "uncaught exception" phase
        (Printexc.to_string e);
      if bt <> "" then Fmt.epr "%s@.@?" bt;
      Analyses.Outcome.exit Fatal

(* Prints the list of testing entry points as a one-line JSON list to stdout, if
   [--list-tests] is set. This is the only thing printed to stdout by [compile],
   so its output can be piped directly into e.g. [jq]. *)
let maybe_list_tests (crate, entry_points) =
  if (Config.get ()).list_tests then
    let fmt_env = Charon.Print.crate_to_fmt_env crate in
    let names =
      List.map
        (fun (ep : Frontend.entry_point) ->
          `String
            (Charon.Print.name_to_string fmt_env ep.fun_decl.item_meta.name))
        entry_points
    in
    print_endline (Yojson.Safe.to_string (`List names))

let do_compilation target =
  let compile () = Frontend.parse_ullbc_with_entry_points target in
  let targets = wrap_step "Compiling" compile in
  maybe_list_tests targets;
  targets

let compile config target =
  let@ () = with_exn_and_config Whole_program config in
  let _ = do_compilation target in
  Analyses.Outcome.Ok

let exec_wpst config target =
  let@ () = with_exn_and_config Whole_program config in
  let targets = do_compilation target in
  Analyses.Wpst.exec targets

let exec_biab config target =
  let@ () = with_exn_and_config Compositional config in
  let compile () = Frontend.parse_ullbc target in
  wrap_step "Compiling" compile |> Analyses.Biab.generate_summaries

let build_plugins config =
  let@ () = with_exn_and_config Whole_program config in
  wrap_step "Compiling plugins" Frontend.compile_all_plugins;
  Ok
