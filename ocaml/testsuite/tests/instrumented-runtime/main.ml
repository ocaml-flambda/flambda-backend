(* TEST
 runtime4;
 instrumented-runtime;
 flags = "-runtime-variant=i";
 native;
*)

(* Test if the instrumented runtime is in working condition *)

[@@@ocaml.alert "-deprecated"]

let _ =
  Gc.eventlog_pause ();
  Gc.eventlog_resume()
