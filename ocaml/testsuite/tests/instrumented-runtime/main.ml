(* TEST
  * runtime4
  ** instrumented-runtime
  *** native
    flags = "-runtime-variant=i"
*)

(* Test if the instrumented runtime is in working condition *)

[@@@ocaml.alert "-deprecated"]

let _ =
  Gc.eventlog_pause ();
  Gc.eventlog_resume()
