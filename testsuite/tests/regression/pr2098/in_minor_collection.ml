(* TEST
 modules = "stub.c";
 not-windows;
 native;
*)

type t
external alloc : unit -> t = "caml_test_alloc"

let () =
  ignore (alloc());
  Gc.minor()