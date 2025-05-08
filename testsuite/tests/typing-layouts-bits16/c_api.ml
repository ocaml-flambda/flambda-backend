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

(* This file tests using external C functions with int16#. *)


external to_int16 : int16# -> int16 = "%tag_int16" [@@warning "-187"]

let print_int16 s f = Printf.printf "%s: %d\n" s (Stdlib_beta.Int16.to_int f)
let print_int16u s f = print_int16 s (to_int16 f)

(* Various combinations of arguments int16, int16 [@unboxed], and
   int16# *)
external lognot_UtoU : int16# -> int16# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_BtoU : int16 -> int16# =
  "lognot_bytecode" "lognot_BtoU"
external lognot_UtoB : int16# -> int16 =
  "lognot_bytecode" "lognot_UtoB"
external lognot_BUtoU : (int16[@unboxed]) -> int16# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_UtoBU : int16# -> (int16[@unboxed]) =
  "lognot_bytecode" "lognot_UtoU"

let () =
  let i = lognot_UtoU (Stdlib_beta.Int16_u.of_int 42) in
  print_int16u "int16# -> int16#, ~42" i

let () =
  let i = lognot_BtoU ((Stdlib_beta.Int16.of_int (-100))) in
  print_int16u "int16 -> int16#, ~(-100)" i

let () =
  let f = lognot_UtoB (Stdlib_beta.Int16_u.of_int 255) in
  print_int16 "int16# -> int16, ~255" f

let () =
  let f = lognot_BUtoU (Stdlib_beta.Int16.of_int 1024) in
  print_int16u "(int16[@unboxed]) -> int16#, ~1024" f

let () =
  let f = lognot_UtoBU ((Stdlib_beta.Int16_u.of_int (-1726))) in
  print_int16 "int16# -> (int16[@unboxed]), ~(-1726)" f

(* If there are more than 5 args, you get an array in bytecode *)
external sum_7 :
  int16# -> int16 -> int16# -> int16 ->
  int16# -> int16 -> int16# -> int16# =
  "sum_7_bytecode" "sum_7_UBUBUBUtoU"

let _ =
  let f =
    sum_7
      (Stdlib_beta.Int16_u.of_int 1) (Stdlib_beta.Int16.of_int 2) (Stdlib_beta.Int16_u.of_int 3) (Stdlib_beta.Int16.of_int 4)
      (Stdlib_beta.Int16_u.of_int 5) (Stdlib_beta.Int16.of_int 6) (Stdlib_beta.Int16_u.of_int 7)
  in
  print_int16u "Function of 7 args, 1+2+3+4+5+6+7" f
