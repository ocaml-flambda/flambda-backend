(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   bytecode;
 } {
   ocamlopt_byte_exit_status = "2";
   setup-ocamlopt.byte-build-env;
   compiler_reference =
     "${test_source_directory}/product_array_marshalling.native.reference";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }
*)

(* Here we check that an attempt to marshal an unboxed product array is
   rejected. *)
(* CR layouts v7.1: This currently hits a middle end exception in native code.
   That will be fixed by the follow-on PR that adds tuple array support to the
   rest of the compiler. This can be moved to beta then. *)
(* CR layouts v4.0: Except it is is not rejected by bytecode.  See the arrays
   epic for our plan to fix that. *)

let array = [| #(1, 2); #(3, 4) |]

let _ = Marshal.to_string array []
