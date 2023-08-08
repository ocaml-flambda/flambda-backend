(* TEST
   flags = "-extension layouts_beta"

   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

(* This file contains various tests for [int64#].  It's not an expect test
   to make sure it gets tested for native code. *)

(* CR layouts: We'll be moving code from [unboxed_int64s_alpha] to this file
   as support for different features becomes available at this level of
   stability. *)

(* No [int64#]s allowed *)
let () =
  let _not_allowed = Stdlib__Int64_u.of_int64 42L in
  ()
