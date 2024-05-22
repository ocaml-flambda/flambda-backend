(* TEST
 flags = "-extension-universe beta";
 include stdlib_beta;
 {
   bytecode;
 }{
   native;
 }
*)

open Stdlib_beta

(* Test that [Float32] is exported. *)

let () =
  let one = Float32.of_float 1.0 in
  assert (Float32.to_float one = 1.0)
;;

(* Test that [Float32_u] is exported. *)

let () =
  let one = Float32_u.of_int 1 in
  assert (Float32_u.to_int one = 1)
;;
