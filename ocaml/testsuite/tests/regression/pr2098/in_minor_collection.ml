(* TEST
   modules = "stub.c"
   include systhreads
   * hassysthreads
   ** not-windows
   *** native
*)

type t
external alloc : unit -> t = "caml_test_alloc"
external init : unit -> unit = "caml_test_init"

let f () =
  init ();
  ignore (alloc ());
  ()

let () =
  f ();
  Gc.minor()