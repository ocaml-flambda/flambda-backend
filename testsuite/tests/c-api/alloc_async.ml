(* TEST
<<<<<<< HEAD
 modules = "alloc_async_stubs.c";
 reason = "alloc async changes: https://github.com/ocaml/ocaml/pull/8897";
 skip;
||||||| 121bedcfd2
   modules = "alloc_async_stubs.c"
   * skip
   reason = "alloc async changes: https://github.com/ocaml/ocaml/pull/8897"
=======
 modules = "alloc_async_stubs.c";
>>>>>>> 5.2.0
*)

external test : int ref -> unit = "stub"
external print_status : string -> int -> unit = "print_status_caml" [@@noalloc]

(* This tests checks that the finaliser does not run during various
   allocations from C, but runs at the first polling location in OCaml
   code after that. For native backends, something like
   RET_FROM_C_CALL from runtime/amd64.S is necessary, see its
   description there and the documentation of
   [Caml_state->action_pending] in runtime.signals.c. *)

let f () =
  let r = ref 42 in
  Gc.finalise (fun s -> r := !s) (ref 17);
  print_status "OCaml, before" !r;
  test r;
  print_status "OCaml, after" !r;
  ignore (Sys.opaque_identity (ref 100));
  print_status "OCaml, after alloc" !r;
  ()

let () = (f [@inlined never]) ()
