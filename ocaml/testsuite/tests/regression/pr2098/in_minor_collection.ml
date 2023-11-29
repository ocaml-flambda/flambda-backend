(* TEST
   modules = "stub.c"
   * hassysthreads
   include systhreads
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

let () = f ()