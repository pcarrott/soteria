(** A simple CFG analysis that tries to decide whether a function is a panic
    helper.

    Bi-abduction will declare any panic helper as a manifest panic, because it
    effectively is. However, we want to remove that noise. To do so, we have a
    trivial dataflow analysis that checks whether all paths go to a panic (i.e.
    never return normally), and if so, we declare the function a panic-helper
    and don't report manifest panics for it. *)

(** [is_never fid] returns true iff no paths of [fundef] lead to a [Return]. The
    goal is to determine whether a function is a panic helper, such as:
    {@rust[
      pub fn handle_error(err: &str) {
          panic!("Error: {}", err);
      }
    ]}

    Summaries are memoised in a global, path-independent cache. *)
val is_never : Charon.Types.FunDeclId.id -> bool
