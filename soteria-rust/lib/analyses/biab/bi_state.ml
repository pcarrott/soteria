module State = State.Tree_state.Make (Tree_borrows.Concrete.Make)
module Bi = Soteria.Sym_states.Bi_abd.Make (Rustsymex)
open State
include Bi (State)

let to_spec st_opt =
  let st, pre = of_opt st_opt in
  (pre, Option.fold ~none:[] ~some:State.to_syn st)

let to_syn _ = failwith "Can't serialize Bi_state"
let wrap f = wrap ~fuel:10 f

let pp_pretty ~ignore_freed ft t =
  pp' ~inner:(State.pp_pretty ~ignore_freed) ft t

let empty = None

let[@inline] load ?ignore_borrow ?ignore_align ptr ty =
  wrap (load ?ignore_borrow ?ignore_align ptr ty)

let[@inline] load_discriminant ?ignore_align ptr ty =
  wrap (load_discriminant ?ignore_align ptr ty)

let[@inline] store ?ignore_align ptr ty v = wrap (store ?ignore_align ptr ty v)
let[@inline] zeros ptr size = wrap (zeros ptr size)
let[@inline] alloc_ty ?span ty = wrap (alloc_ty ?span ty)

let[@inline] alloc_untyped ?span ~zeroed ~size ~align =
  wrap (alloc_untyped ?span ~zeroed ~size ~align)

let[@inline] copy_nonoverlapping ~src ~dst ~size =
  wrap (copy_nonoverlapping ~src ~dst ~size)

let[@inline] transmute ~from ~to_ v = wrap (transmute ~from ~to_ v)
let[@inline] transmute_raw ~to_ v = wrap (transmute_raw ~to_ v)
let[@inline] free ptr = wrap (free ptr)
let[@inline] borrow ?protect ptr ty = wrap (borrow ?protect ptr ty)
let[@inline] unprotect ptr ty = wrap (unprotect ptr ty)
let[@inline] with_exposed addr = wrap (with_exposed addr)
let[@inline] tb_load ptr ty = wrap (tb_load ptr ty)
let[@inline] load_global g = wrap (load_global g)
let[@inline] store_global g ptr = wrap (store_global g ptr)
let[@inline] load_str_global str = wrap (load_str_global str)
let[@inline] store_str_global str ptr = wrap (store_str_global str ptr)
let[@inline] declare_fn fn = wrap (declare_fn fn)
let[@inline] lookup_fn fn = wrap (lookup_fn fn)
let[@inline] lookup_const_generic id ty = wrap (lookup_const_generic id ty)
let[@inline] add_error e = wrap (add_error e)
let[@inline] leak_check () = wrap (leak_check ())
let[@inline] run_thread_exits () = wrap (run_thread_exits ())
let[@inline] pop_error () = wrap (pop_error ())
let[@inline] with_pointers_sym x = wrap_no_fail (with_pointers_sym x)
let[@inline] fake_read ptr ty = wrap_no_fail (fake_read ptr ty)
let[@inline] size_and_align_of_val ty ptr = wrap (size_and_align_of_val ty ptr)
let[@inline] check_non_dangling ptr ty = wrap (check_non_dangling ptr ty)
let[@inline] type_id ty = wrap (type_id ty)

let[@inline] register_thread_exit
    (f : unit -> (unit, Error.with_trace, syn list) SM.Result.t) =
  let unlifted () : (unit, Error.with_trace, syn list) State.SM.Result.t =
   fun st ->
    (* HACK: is this sound ? *)
    let open Rustsymex.Syntax in
    let+ res, st = f () (Some (st, [])) in
    (res, Option.bind fst st)
  in
  wrap (register_thread_exit unlifted)

module Sptr = struct
  include State.Sptr

  let offset ?check_signed ?ty ofs ptr = wrap (offset ?check_signed ?ty ofs ptr)
  let check_non_dangling ptr ty = wrap (check_non_dangling ptr ty)

  let check_non_dangling_untyped ptr size =
    wrap (check_non_dangling_untyped ptr size)

  let check_aligned ptr ty = wrap (check_aligned ptr ty)
end
