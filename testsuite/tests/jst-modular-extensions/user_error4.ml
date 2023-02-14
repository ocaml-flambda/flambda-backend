(* TEST
   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

(* What happens if the user tries to write one of the ocaml-jst extensions in
   terms of extension nodes but messes up?  In practice we don't expect to ever
   see these errors, but one never knows (and a bug in our desugaring could
   cause them).  The let-binding is named after the constructor in
   [extensions.ml] representing this particular error. *)

(* We cannot use an expect-test here, because these are essentially parsing
   errors. The expect-test infrastructure uses Ast_mapper to prepare its
   input, and the call to Ast_mapper fails with a bogus extension setup,
   because it tries to decode extensions. We thus have this error and others
   like it in separate files, because the "compile and test output"
   infrastructure reports only one error at a time. *)

let _disabled_extension = [%extension.comprehensions_experimental] ();;

(*
Line 1, characters 26-69:
1 | let _disabled_extension = [%extension.comprehensions_experimental] ();;
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The extension "comprehensions_experimental" is disabled and cannot be used
*)


