(* TEST
   flags = "-extension layouts"

   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

(* This file contains various tests for [int32#].  It's not an expect test
   to make sure it gets tested for native code. *)

(* CR layouts: We'll be moving code from [unboxed_int32s_beta] to this file
   as support for different features becomes available at this level of
   stability. *)

(* No [int32#]s allowed *)
let () =
  let _not_allowed = Stdlib__Int32_u.of_int32 42l in
  ()
