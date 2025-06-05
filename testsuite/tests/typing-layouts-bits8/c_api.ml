(* TEST
 modules = "c_functions.c";
 include stdlib_beta;
 flambda2;
 {
   flags = "-extension layouts_alpha -extension small_numbers_beta";
   native;
 }{
   flags = "-extension layouts_alpha -extension small_numbers_beta";
   bytecode;
 }{
   flags = "-extension layouts_beta -extension small_numbers_beta";
   native;
 }{
   flags = "-extension layouts_beta -extension small_numbers_beta";
   bytecode;
 }
*)

(* This file tests using external C functions with int8#. *)


external to_int8 : int8# -> int8 = "%tag_int8" [@@warning "-187"]

let print_int8 s f = Printf.printf "%s: %d\n" s (Stdlib_beta.Int8.to_int f)
let print_int8u s f = print_int8 s (to_int8 f)

(* Various combinations of arguments int8, int8 [@unboxed], and
   int8# *)
external lognot_UtoU : int8# -> int8# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_BtoU : int8 -> int8# =
  "lognot_bytecode" "lognot_BtoU"
external lognot_UtoB : int8# -> int8 =
  "lognot_bytecode" "lognot_UtoB"
external lognot_BUtoU : (int8[@unboxed]) -> int8# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_UtoBU : int8# -> (int8[@unboxed]) =
  "lognot_bytecode" "lognot_UtoU"

let () =
  let i = lognot_UtoU (Stdlib_beta.Int8_u.of_int 42) in
  print_int8u "int8# -> int8#, ~42" i

let () =
  let i = lognot_BtoU ((Stdlib_beta.Int8.of_int (-100))) in
  print_int8u "int8 -> int8#, ~(-100)" i

let () =
  let f = lognot_UtoB (Stdlib_beta.Int8_u.of_int 255) in
  print_int8 "int8# -> int8, ~255" f

let () =
  let f = lognot_BUtoU (Stdlib_beta.Int8.of_int 28) in
  print_int8u "(int8[@unboxed]) -> int8#, ~28" f

let () =
  let f = lognot_UtoBU ((Stdlib_beta.Int8_u.of_int (-26))) in
  print_int8 "int8# -> (int8[@unboxed]), ~(-26)" f

(* If there are more than 5 args, you get an array in bytecode *)
external sum_7 :
  int8# -> int8 -> int8# -> int8 ->
  int8# -> int8 -> int8# -> int8# =
  "sum_7_bytecode" "sum_7_UBUBUBUtoU"

let _ =
  let f =
    sum_7
      (Stdlib_beta.Int8_u.of_int 1) (Stdlib_beta.Int8.of_int 2) (Stdlib_beta.Int8_u.of_int 3) (Stdlib_beta.Int8.of_int 4)
      (Stdlib_beta.Int8_u.of_int 5) (Stdlib_beta.Int8.of_int 6) (Stdlib_beta.Int8_u.of_int 7)
  in
  print_int8u "Function of 7 args, 1+2+3+4+5+6+7" f
