type uncaught_in = Generating_summary of string | Analysing_summary of string

exception Uncaught_exception of uncaught_in * exn

let[@inline] with_reraised ~uncaught_in f =
  try f ()
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace (Uncaught_exception (uncaught_in (), e)) bt
