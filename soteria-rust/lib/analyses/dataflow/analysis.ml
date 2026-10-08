open Charon
open Iarray.Infix

module Fn_tbl = Hashtbl.Make (struct
  open Types.FunDeclId

  type t = id

  let equal = Types.FunDeclId.equal_id
  let hash i = Int.hash (to_int i)
end)

(* The CFG successors of a terminator, including unwind/secondary targets. *)
let successors_of (k : UllbcAst.terminator_kind) : UllbcAst.BlockId.id list =
  match k with
  | Goto t -> [ t ]
  | Switch (_, If (b1, b2)) -> [ b1; b2 ]
  | Switch (_, SwitchInt (_, cases, otherwise)) ->
      otherwise :: List.map snd cases
  | Call (_, t, u) -> [ t; u ]
  | Drop (_, _, _, t, u) -> [ t; u ]
  | TAssert (_, t, u) -> [ t; u ]
  | InlineAsm (_, ts, u) -> u :: ts
  | Abort _ | Return | UnwindResume -> []

let entry_block = UllbcAst.BlockId.(to_int zero)

(* Reverse of an edge relation: [reverse n edges].(j) is the list of [i] with
   [j] in [edges.(i)]. *)
let reverse n (edges : int list iarray) =
  let rev = Array.make n [] in
  Iarray.iteri
    (fun i js -> List.iter (fun j -> rev.(j) <- i :: rev.(j)) js)
    edges;
  rev

(* A generic worklist least-fixpoint over the nodes of [values] (pre-seeded with
   the least element). [step i] recomputes node [i] from its neighbours' current
   values; [deps.(i)] are the nodes to re-enqueue when [i]'s value changes;
   [order] is the initial visiting order (only affects convergence speed). *)
let worklist ~order ~deps ~equal ~step values =
  let n = Array.length values in
  let queued = Array.make n false in
  let wl = Queue.create () in
  let push i =
    if not queued.(i) then (
      queued.(i) <- true;
      Queue.push i wl)
  in
  Iter.iter push order;
  while not (Queue.is_empty wl) do
    let i = Queue.pop wl in
    queued.(i) <- false;
    let v = step i in
    if not (equal values.(i) v) then (
      values.(i) <- v;
      List.iter push deps.(i))
  done

(** The direction-specific ingredients of an analysis, independent of the
    domain: which CFG edges feed a block's input, where the boundary is seeded,
    how the transfer is threaded, and where the function summary is read off. *)
module type Direction = sig
  (** The in-edges feeding each block's input, given a map from a block to its
      live CFG successors: predecessors (forward) or successors (backward). *)
  val in_edges : int -> (int -> int list) -> int list iarray

  (** [base ~boundary ~bottom in_edges i] seeds block [i]: [boundary] at a
      source block, [bottom] elsewhere. *)
  val base : boundary:'a -> bottom:'a -> int list iarray -> int -> 'a

  (** Thread a block's transfers in the analysis direction. *)
  val transfer :
    statement:('a -> UllbcAst.statement -> 'a) ->
    terminator:('a -> UllbcAst.terminator_kind -> 'a) ->
    'a ->
    UllbcAst.block ->
    'a

  (** The order in which to seed the block worklist; affects only convergence
      speed. *)
  val order : int -> int Iter.t

  (** The function summary: the block output fact at the analysis's sink. *)
  val summary_of_out :
    join:('a -> 'a -> 'a) ->
    bottom:'a ->
    UllbcAst.block iarray ->
    'a iarray ->
    'a
end

module Forward : Direction = struct
  open UllbcAst

  let in_edges n live =
    let preds = Array.make n [] in
    for i = 0 to n - 1 do
      List.iter (fun j -> preds.(j) <- i :: preds.(j)) (live i)
    done;
    Iarray.of_array preds

  let base ~boundary ~bottom _in_edges i =
    if i = entry_block then boundary else bottom

  let transfer ~statement ~terminator input blk =
    terminator
      (List.fold_left statement input blk.statements)
      blk.terminator.kind

  let order n = Iter.(0 -- (n - 1))

  (* The fact on return is the join of the exit facts of the return blocks. *)
  let summary_of_out ~join ~bottom blocks out =
    let s = ref bottom in
    Iarray.iteri
      (fun i blk ->
        match blk.terminator.kind with
        | Return -> s := join !s out.%(i)
        | _ -> ())
      blocks;
    !s
end

module Backward : Direction = struct
  open UllbcAst

  let in_edges n live = Iarray.init n live

  (* The source blocks are the exits: those with no successors. *)
  let base ~boundary ~bottom in_edges i =
    match in_edges.%(i) with [] -> boundary | _ -> bottom

  let transfer ~statement ~terminator input blk =
    List.fold_left statement
      (terminator input blk.terminator.kind)
      (List.rev blk.statements)

  let order n = Iter.(n - 1 --^ 0)
  let summary_of_out ~join:_ ~bottom:_ _blocks out = out.%(entry_block)
end

(** A single abstract domain: the lattice and per-instruction transfers. A
    transfer is just [input fact -> output fact] and never sees the graph;
    {e where} an instruction's input comes from is the {!DIRECTION}'s concern.
*)
module Domain = struct
  include Intf.Domain

  module Default (L : Lattice) : S with type t = L.t = struct
    include L

    let statement t _ = t
    let terminator ~summary:_ t _ = t
    let live_successors ~fn:_ ~block:_ kind = successors_of kind
    let opaque _ = bottom
    let known_summary _ = None
  end
end

module type S = Intf.S

module Make (Dir : Direction) (Dom : Domain.S) = struct
  open UllbcAst

  type t = Dom.t

  (* Least fixpoint of one function's per-block facts, using [summary] for the
     interprocedural facts of calls. Returns the output fact of every block (its
     exit fact forwards, its entry fact backwards). *)
  let analyse_blocks ~summary ~fn (blocks : block iarray) : Dom.t iarray =
    let n = Iarray.length blocks in
    let live i =
      Dom.live_successors ~fn ~block:i blocks.%(i).terminator.kind
      |> List.map BlockId.to_int
    in
    let in_edges = Dir.in_edges n live in
    let deps = reverse n in_edges in
    let out = Array.make n Dom.bottom in
    let step i =
      let input =
        List.fold_left
          (fun acc j -> Dom.join acc out.(j))
          (Dir.base ~boundary:Dom.boundary ~bottom:Dom.bottom in_edges i)
          in_edges.%(i)
      in
      Dir.transfer ~statement:Dom.statement
        ~terminator:(Dom.terminator ~summary) input blocks.%(i)
    in
    worklist ~order:(Dir.order n) ~deps ~equal:Dom.equal ~step out;
    Iarray.of_array out

  let summary_of_out blocks out =
    Dir.summary_of_out ~join:Dom.join ~bottom:Dom.bottom blocks out

  (* Global, path-independent memo of the (pure) summary and per-block facts of
     each function. Sound to share across queries: a context-insensitive summary
     depends only on the crate, never on the caller.

     TODO: not thread-safe. *)
  let summaries : Dom.t Fn_tbl.t = Fn_tbl.create 0
  let facts_of : Dom.t iarray Fn_tbl.t = Fn_tbl.create 0

  (* The mutable state of one call-graph fixpoint query: the in-progress
     summaries and facts, the discovered caller edges, and the worklist. *)
  type state = {
    work : Dom.t Fn_tbl.t;
    work_facts : Dom.t iarray Fn_tbl.t;
    callers : Types.FunDeclId.id list Fn_tbl.t;
    queued : unit Fn_tbl.t;
    wl : Types.FunDeclId.id Queue.t;
  }

  let push st fid =
    if not (Fn_tbl.mem st.queued fid) then (
      Fn_tbl.replace st.queued fid ();
      Queue.push fid st.wl)

  (* The current best summary of [fid]: a finalized one if known, else the
     in-progress value, else [⊥] for a not-yet-seen (e.g. recursive) callee. *)
  let current st fid =
    match Fn_tbl.find_opt summaries fid with
    | Some v -> v
    | None -> Option.value ~default:Dom.bottom (Fn_tbl.find_opt st.work fid)

  (* Schedule [fid] for analysis if it isn't finalized or in progress yet. *)
  let ensure st fid =
    if (not (Fn_tbl.mem summaries fid)) && not (Fn_tbl.mem st.work fid) then (
      Fn_tbl.replace st.work fid Dom.bottom;
      push st fid)

  (* The interprocedural summary of [c]'s callee, registering the caller edge
     and scheduling the callee so [decl] is revisited when it grows. *)
  let call_summary st (decl : fun_decl) (c : call) : Dom.t =
    match c.func with
    | FnOpRegular { kind = FunId (FRegular callee); _ } ->
        Fn_tbl.replace st.callers callee
          (decl.def_id
          :: Option.value ~default:[] (Fn_tbl.find_opt st.callers callee));
        ensure st callee;
        current st callee
    | FnOpRegular { kind = FunId (FBuiltin _); _ }
    | FnOpRegular { kind = TraitMethod _; _ }
    | FnOpDynamic _ ->
        (* Dynamic calls, method calls, builtins: not descended into. *)
        Dom.opaque decl

  (* Re-analyse one function; if its summary grew, re-enqueue its callers. *)
  let process st fid =
    let decl = Crate.get_fun fid in
    let summary = call_summary st decl in
    let facts, result =
      match Dom.known_summary decl with
      | Some v -> (Iarray.init 0 (fun _ -> assert false), v)
      | None -> (
          match decl.body with
          | UnstructuredBody b when not (List.is_empty b.body) ->
              let blocks = Iarray.of_list b.body in
              let out = analyse_blocks ~summary ~fn:fid blocks in
              (out, summary_of_out blocks out)
          | _ ->
              [%l.trace
                "No body for %a, assuming opaque" Crate.pp_name
                  decl.item_meta.name];
              ([||], Dom.opaque decl))
    in
    Fn_tbl.replace st.work_facts fid facts;
    if not (Dom.equal result (current st fid)) then (
      Fn_tbl.replace st.work fid result;
      List.iter (push st)
        (Option.value ~default:[] (Fn_tbl.find_opt st.callers fid)))

  (* Compute (and memoise) the summary and per-block facts of [root] and every
     function transitively reachable from it, as the global least fixpoint over
     the call graph. Recursion needs no special handling: a not-yet-computed
     callee simply reads as [⊥]. *)
  let run root =
    if not (Fn_tbl.mem summaries root) then begin
      let st =
        {
          work = Fn_tbl.create 16;
          work_facts = Fn_tbl.create 16;
          callers = Fn_tbl.create 16;
          queued = Fn_tbl.create 16;
          wl = Queue.create ();
        }
      in
      ensure st root;
      while not (Queue.is_empty st.wl) do
        let fid = Queue.pop st.wl in
        Fn_tbl.remove st.queued fid;
        process st fid
      done;
      Fn_tbl.iter (Fn_tbl.replace summaries) st.work;
      Fn_tbl.iter (Fn_tbl.replace facts_of) st.work_facts
    end

  let analyse_fun fid =
    run fid;
    Option.value ~default:[||] (Fn_tbl.find_opt facts_of fid)

  let summary fid =
    run fid;
    match Fn_tbl.find_opt summaries fid with
    | Some v -> v
    | None -> Dom.opaque (Crate.get_fun fid)
end

module Make_forward (Dom : Domain.S) = Make (Forward) (Dom)
module Make_backward (Dom : Domain.S) = Make (Backward) (Dom)
