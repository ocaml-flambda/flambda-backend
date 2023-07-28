(* TEST
   modules = "c_functions.c"
   flags = "-extension layouts_alpha"
*)

(* This file tests using external C functions with int64#. *)

external to_int64 : int64# -> (int64[@local_opt]) = "%box_int64"
external of_int64 : (int64[@local_opt]) -> int64# =
  "%unbox_int64"

let print_int64u s f = Printf.printf "%s: %Ld\n" s (to_int64 f)
let print_int64 s f = Printf.printf "%s: %Ld\n" s f

(* Various combinations of arguments int64, int64 [@unboxed], and
   int64# *)
external lognot_UtoU : int64# -> int64# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_BtoU : int64 -> int64# =
  "lognot_bytecode" "lognot_BtoU"
external lognot_UtoB : int64# -> int64 =
  "lognot_bytecode" "lognot_UtoB"
external lognot_BUtoU : (int64[@unboxed]) -> int64# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_UtoBU : int64# -> (int64[@unboxed]) =
  "lognot_bytecode" "lognot_UtoU"

let () =
  let i = lognot_UtoU (of_int64 42L) in
  print_int64u "int64# -> int64#, ~42" i

let () =
  let i = lognot_BtoU (-100L) in
  print_int64u "int64 -> int64#, ~(-100)" i

let () =
  let f = lognot_UtoB (of_int64 255L) in
  print_int64 "int64# -> int64, ~255" f

let () =
  let f = lognot_BUtoU 1024L in
  print_int64u "(int64[@unboxed]) -> int64#, ~1024" f

let () =
  let f = lognot_UtoBU (of_int64 (-1726L)) in
  print_int64 "int64# -> (int64[@unboxed]), ~(-1726)" f

(* If there are more than 5 args, you get an array in bytecode *)
external sum_7 :
  int64# -> int64 -> int64# -> int64 ->
  int64# -> int64 -> int64# -> int64# =
  "sum_7_bytecode" "sum_7_UBUBUBUtoU"

let _ =
  let f =
    sum_7
      (of_int64 1L) 2L (of_int64 3L) 4L
      (of_int64 5L) 6L (of_int64 7L)
  in
  print_int64u "Function of 7 args, 1+2+3+4+5+6+7" f


