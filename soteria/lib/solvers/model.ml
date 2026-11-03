(** Reading models off an SMT solver. *)

(** A variable [var] of type [ty] that holds a value once the constraint [eq] is
    added, and how to [read] that value back from the value of [var] in a model.
*)
type ('ty, 'value, 'v) binding = {
  var : Symex.Var.t;
  ty : 'ty;
  eq : 'value;
  read : Smt.sexp -> 'v option;
}

module Make (Intf : Solver_interface.S) = struct
  module Var = Symex.Var

  (** [model solver ~iter_vars ~bind pcs] adds [pcs] to the constraints already
      sent to [solver], and returns the value in a model of every variable of
      [pcs] that [bind] can bind, or [None] if no model is found.
      [bind var v ty] binds the variable [v] of type [ty] to the fresh variable
      [var]. *)
  let model solver ~iter_vars ~bind (pcs : Intf.value list) :
      'v Var.Map.t option =
    let vars =
      Iter.of_list pcs
      |> Iter.flat_map iter_vars
      |> Iter.fold (fun vars (v, ty) -> Var.Map.add v ty vars) Var.Map.empty
    in
    Var.Map.iter (Intf.declare_var solver) vars;
    List.iter (Intf.add_constraint solver) pcs;
    let fresh = Var.Map.fold (fun v _ n -> max n (Var.to_int v + 1)) vars 0 in
    let _, bindings =
      Var.Map.fold
        (fun v ty (next, bindings) ->
          match bind (Var.of_int next) v ty with
          | Some binding -> (next + 1, (v, binding) :: bindings)
          | None -> (next, bindings))
        vars (fresh, [])
    in
    ListLabels.iter bindings ~f:(fun (_, { var; ty; eq; _ }) ->
        Intf.declare_var solver var ty;
        Intf.add_constraint solver eq);
    match (Intf.check_sat solver, Intf.get_model solver) with
    | Sat, Some (List defs) ->
        let value_of var : Smt.sexp -> Smt.sexp option = function
          | List [ Atom "define-fun"; Atom name; List []; _; value ]
            when name = Var.to_string var ->
              Some value
          | _ -> None
        in
        ListLabels.fold_left bindings ~init:(Some Var.Map.empty)
          ~f:(fun model (v, { var; read; _ }) ->
            Option.bind model (fun model ->
                Option.bind (List.find_map (value_of var) defs) read
                |> Option.map (fun value -> Var.Map.add v value model)))
    | _ -> None
end
