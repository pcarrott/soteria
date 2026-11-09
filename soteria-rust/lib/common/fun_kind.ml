type synth_fn =
  | GenericDropInPlace
  | DropOpaque of
      (Charon.Types.type_var_id
      [@compare Charon.Types.TypeVarId.compare_id]
      [@printer Charon.Types.pp_type_var_id])
      (** Drops the opaque value of a type variable *)
[@@deriving ord, show { with_path = false }]

type t =
  | Real of Charon.Types.fun_decl_ref [@printer Crate.pp_fun_decl_ref]
  | Synthetic of synth_fn
[@@deriving ord, show { with_path = false }]
