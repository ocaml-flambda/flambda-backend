(* TEST

   flags = "-w +A-60-70"

 * setup-ocamlc.byte-build-env
 ** ocamlc.byte
   compile_only = "true"
 *** check-ocamlc.byte-output

*)

(*
 If we use attributes on a syntactic category not handled by modular syntax,
 they aren't interpreted by the modular syntax machinery and fail with a normal
 OCaml error.

 We may some day run out of such syntactic categories, in which case we should
 delete this test and leave a comment saying that we can't write such a test for
 attributes because the modular syntax machinery always interprets them.
*)
let[@jane] f () = ();;

(* We can't use expect test here because warning 53 is only raised by ocamlc,
   not the toplevel. This is probably a bug.
*)
