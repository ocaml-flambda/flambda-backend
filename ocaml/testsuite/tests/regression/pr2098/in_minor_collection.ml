(* TEST
   modules = "stub.c"
   * not-windows
   ** native
*)

type t
external alloc : unit -> t = "caml_test_alloc"
external init : unit -> unit = "caml_test_init"

let () =
  init ();
  ignore (alloc());
  Gc.minor()