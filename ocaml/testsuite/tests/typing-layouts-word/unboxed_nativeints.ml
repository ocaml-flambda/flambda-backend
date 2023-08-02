(* TEST
   flags = "-extension layouts"

   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

(* This file contains various tests for [nativeint#].  It's not an expect test
   to make sure it gets tested for native code. *)

(* CR layouts: We'll be moving code from [unboxed_nativeints_beta] to this file
   as support for different features becomes available at this level of
   stability. *)

(* No [nativeint#]s allowed *)
let () =
  let _not_allowed = Stdlib__Nativeint_u.of_nativeint 42n in
  ()
