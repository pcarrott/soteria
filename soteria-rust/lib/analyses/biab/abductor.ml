open Syntaxes.FunctionWrap
open Soteria.Terminal
open Svalue
module State = Bi_state
module Interp = Interp.Make (State)

module Summaries = struct
  module H = Hashtbl.Make (struct
    type t = Charon.Types.fun_decl_ref

    let hash = Hashtbl.hash
    let equal = Charon.Types.equal_fun_decl_ref
  end)
end

let default_abductor_fuel =
  Soteria.Symex.Fuel_gauge.{ steps = Finite 5000; branching = Finite 5 }

(** Generates summaries for a function given a function definitions. Has to be
    run within {{!Soteria.Stats.As_ctx.with_stats}with_stats} and
    {!Crate.with_crate} *)
let generate_summaries_for (fundef : Charon.UllbcAst.fun_decl) =
  let open Syntaxes.List in
  let section_name =
    Fmt.str "Generate summaries for %a" Crate.pp_name fundef.item_meta.name
  in
  let@ () = L.with_section section_name in
  [%l.info "%s" section_name];
  let arg_tys = fundef.signature.inputs in
  let process =
    let open Rustsymex in
    let open Rustsymex.Syntax in
    (* The layouts chosen for the arguments are kept in the initial state *)
    let* args, state =
      State.wrap_no_fail
        (State.State.lift_layout
           (Layout.SM.Result.map_list arg_tys ~f:Value_codec.nondet_valid))
        State.empty
    in
    let* args = match args with Ok args -> return args | _ -> vanish () in
    let* result = Interp.exec_fun_compo ~args ~state fundef in
    match result with
    | Ok (ret, state) -> return (args, Ok ret, state)
    | Error (err, state) -> return (args, Error err, state)
    | Missing _ -> vanish ()
  in
  let res =
    let@ () = L.with_section "Running symbolic execution" in
    Rustsymex.run ~stats:Caller ~mode:UX ~fuel:default_abductor_fuel process
  in
  let+ (args, ret, bi_state), pc = res in
  let args = List.map Typed.Expr.of_value args in
  let ret = Result.map Typed.Expr.of_value ret in
  let@ () = L.with_section "Building summary" in
  [%l.trace
    "@[<2>Building summary for %a using bistate:@ %a@]" Crate.pp_name
      fundef.item_meta.name (Fmt.Dump.option State.pp) bi_state];
  let pre, post = State.to_spec bi_state in
  let ret = Summary.make ~args ~ret ~pre ~post ~pc () in
  [%l.debug "Obtained summary: %a" Summary.pp ret];
  ret

let generate_all_summaries to_analyse =
  let@ () =
    Progress_bar.run ~msg:"Generating summaries" ~total:(List.length to_analyse)
      ()
  in
  let@ () = Soteria.Stats.As_ctx.with_ () in
  ListLabels.map to_analyse ~f:(fun (fn : Charon.GAst.fun_decl) ->
      let summaries =
        Uncaught.with_reraised
          ~uncaught_in:(fun () ->
            let fname = Fmt.to_to_string Crate.pp_name fn.item_meta.name in
            Generating_summary fname)
          (fun () -> generate_summaries_for fn)
      in
      Progress_bar.signal_progress 1;
      (fn, summaries))
