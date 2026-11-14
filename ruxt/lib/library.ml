open Charon
open Soteria.Soteria_std
open Syntaxes.FunctionWrap
module Config_ = Config
module Interp_ = Interp
open Soteria_rust_lib
module Config = Config_
module Interp = Interp_
open Result.Syntax

type t = {
  constructors : (Wrapper.t * Types.ty list) list;
  fun_decls : (Wrapper.t * Types.ty list) list;
}

let get () =
  let crate = Crate.get_crate () in
  (* Trait methods can be called by safe code, except for [Drop::drop]; closures
     are not part of the API *)
  let is_callable : UllbcAst.fun_source -> bool = function
    | NormalFun -> true
    | TraitImplFun (impl_ref, decl, _, _) -> (
        (Crate.get_trait_decl decl).item_meta.lang_item
        <> Some RustcLangItemDrop
        &&
        match (Crate.get_trait_impl_raw impl_ref.id).src with
        | ClosureTraitImpl _ -> false
        | _ -> true)
    | _ -> false
  in
  (* Const generics and type variables with trait bounds, other than the
     implicit [Destruct] bound, are not supported yet *)
  let is_supported_generics (generics : Types.generic_params) =
    List.is_empty generics.const_generics
    && List.for_all
         (fun (clause : Types.trait_param) ->
           let decl = Crate.get_trait_decl clause.trait.binder_value in
           decl.item_meta.lang_item = Some RustcLangItemDestruct)
         generics.trait_clauses
  in
  let can_infer ({ src; item_meta; signature; generics; _ } : UllbcAst.fun_decl)
      =
    is_callable src
    && is_supported_generics generics
    && item_meta.is_local
    && (not signature.is_unsafe)
    && (item_meta.attr_info.public || not (Config.get ()).only_public)
  in
  let is_constructor ({ signature = { inputs; _ }; _ } : UllbcAst.fun_decl) =
    List.for_all Summary.Context.is_base_ty inputs
  in
  let update_fun_decls (fun_decl : UllbcAst.fun_decl) (constructors, fun_decls)
      =
    if is_constructor fun_decl then (fun_decl :: constructors, fun_decls)
    else (constructors, fun_decl :: fun_decls)
  in
  let constructors, fun_decls =
    Types.FunDeclId.Map.fold
      (fun _ (fun_decl : UllbcAst.fun_decl) library ->
        if Drop.is_drop fun_decl then library
        else if can_infer fun_decl then
          let signature = Common.Charon_util.entry_signature fun_decl in
          update_fun_decls { fun_decl with signature } library
        else library)
      crate.fun_decls ([], [])
  in
  let drops = Drop.get (constructors @ fun_decls) in
  let wrap = List.map (Wrapper.make drops) in
  { constructors = wrap constructors; fun_decls = wrap fun_decls }

let infer_summaries ~fuel summ_ctx wrappers =
  (* Infer summaries and prune summary context *)
  let+ summ_ctx =
    Monad.ResultM.fold_list wrappers ~init:summ_ctx
      ~f:(fun summ_ctx (wrapper, tys) ->
        let+ summ_ctx =
          let@ () =
            L.with_section
              (Fmt.str "Inferring summary for %a" Crate.pp_name
                 (Wrapper.name wrapper))
          in
          (* Iterate over snapshot of current summary context *)
          let snapshot = Summary.Context.iter_summs tys summ_ctx in

          Monad.ResultM.fold_iter snapshot ~init:summ_ctx
            ~f:(fun summ_ctx inputs ->
              (* Stage update to summary context with inferred summaries *)
              let+ outputs = Wrapper.exec ~fuel wrapper inputs in
              ListLabels.fold_left outputs ~init:summ_ctx
                ~f:(fun ctx (ty, summ) -> Summary.Context.stage ty summ ctx))
        in
        L.with_section "Current summary context" (fun () ->
            L.debug (fun m -> m "%a" Summary.Context.pp summ_ctx));
        summ_ctx)
  in
  (* Commit update with new summaries *)
  Summary.Context.commit summ_ctx

let init_summaries ~fuel library =
  infer_summaries ~fuel Summary.Context.empty library.constructors

let infer_summaries ~fuel summ_ctx library =
  infer_summaries ~fuel summ_ctx library.fun_decls
