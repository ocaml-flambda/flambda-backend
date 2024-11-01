(* TEST
 flags = "-extension-universe upstream_compatible";
 include stdlib_upstream_compatible;
 {
   bytecode;
 }{
   native;
 }
*)

open Stdlib_upstream_compatible

(* Test that [Float_u] is exported. *)

let () =
  let pi = Float_u.of_float 3.14 in
  assert (Float_u.to_float pi = 3.14)
;;

(* Test that [Int32_u] is exported. *)

let () =
  let x = Int32_u.of_int 42 in
  assert (Int32_u.to_int x = 42)
;;

(* Test that [Int64_u] is exported. *)

let () =
  let x = Int64_u.of_int 42 in
  assert (Int64_u.to_int x = 42)
;;

(* Test that [Nativeint_u] is exported. *)

let () =
  let x = Nativeint_u.of_int 42 in
  assert (Nativeint_u.to_int x = 42)
;;
