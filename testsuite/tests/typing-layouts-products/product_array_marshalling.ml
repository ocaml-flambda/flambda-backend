(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   bytecode;
 } {
   exit_status = "2";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
 }
*)

(* Here we check that an attempt to marshal an unboxed product array is
   rejected at runtime. *)
(* CR mshinwell: move this to beta once blit is ready, and the associated tests *)
(* CR layouts v4.0: Except it is is not rejected by bytecode.  See the arrays
   epic for our plan to fix that. *)

let array = [| #(1, 2); #(3, 4) |]

let _ = Marshal.to_string array []
