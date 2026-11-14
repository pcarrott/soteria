module Charon = struct
  include Charon

  module Types = struct
    include Charon.Types

    let map_ty (f : type_var_id -> type_var_id) =
      let visitor =
        object
          inherit [_] map_ty

          method! visit_TVar _ =
            function Free id -> TVar (Free (f id)) | Bound _ as v -> TVar v
        end
      in
      visitor#visit_ty ()

    let iter_ty (ty : ty) f =
      let visitor =
        object
          inherit [_] iter_ty
          method! visit_TVar _ = function Free id -> f id | Bound _ -> ()
        end
      in
      visitor#visit_ty () ty

    let mentions id (ty : ty) =
      let exception Found in
      try
        iter_ty ty (fun id' -> if TypeVarId.equal_id id id' then raise Found);
        false
      with Found -> true

    (* Regions and trait references do not distinguish the values of a type *)
    let erase =
      let visitor =
        object (self)
          inherit [_] map_ty
          method! visit_region _ _ = RErased

          method! visit_generic_args env generics =
            {
              generics with
              regions = [];
              types = List.map (self#visit_ty env) generics.types;
              trait_refs = [];
            }
        end
      in
      visitor#visit_ty ()

    (* Numbers type variables from 0, in order of first occurrence. *)
    let numbering () : type_var_id -> type_var_id =
      let seen = ref [] in
      fun id ->
        match List.assoc_opt id !seen with
        | Some n -> n
        | None ->
            let n = TypeVarId.of_int (List.length !seen) in
            seen := (id, n) :: !seen;
            n

    (* The renaming of the type variables of [ty] into those of its canonical
       form. *)
    let to_canonical ty =
      let number = numbering () in
      iter_ty (erase ty) (fun id -> ignore (number id));
      number

    (* The renaming of the type variables of the canonical form of [ty] back
       into those of [ty]. Other type variables are unrelated to those of [ty]:
       they are moved out of their way. *)
    let of_canonical ty =
      let number = numbering () in
      let bindings = ref [] in
      iter_ty (erase ty) (fun id -> bindings := (number id, id) :: !bindings);
      fun id ->
        match List.assoc_opt id !bindings with
        | Some id -> id
        | None -> TypeVarId.of_int (0x10000 + TypeVarId.to_int id)

    (* The canonical form of [ty] is unique up to a one-to-one renaming of type
       variables. *)
    let canonical ty = map_ty (to_canonical ty) (erase ty)
  end
end
