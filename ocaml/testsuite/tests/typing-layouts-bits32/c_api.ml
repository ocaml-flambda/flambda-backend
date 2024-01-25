(* TEST
   modules = "c_functions.c"
   * flambda2
   ** native
   flags = "-extension layouts_alpha"
   ** bytecode
   flags = "-extension layouts_alpha"
   ** native
   flags = "-extension layouts_beta"
   ** bytecode
   flags = "-extension layouts_beta"
   ** native
   ** bytecode
*)

(* This file tests using external C functions with int32#. *)

external to_int32 : int32# -> (int32[@local_opt]) = "%box_int32"

let print_int32u s f = Printf.printf "%s: %ld\n" s (to_int32 f)
let print_int32 s f = Printf.printf "%s: %ld\n" s f

(* Various combinations of arguments int32, int32 [@unboxed], and
   int32# *)
external lognot_UtoU : int32# -> int32# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_BtoU : int32 -> int32# =
  "lognot_bytecode" "lognot_BtoU"
external lognot_UtoB : int32# -> int32 =
  "lognot_bytecode" "lognot_UtoB"
external lognot_BUtoU : (int32[@unboxed]) -> int32# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_UtoBU : int32# -> (int32[@unboxed]) =
  "lognot_bytecode" "lognot_UtoU"

let () =
  let i = lognot_UtoU #42l in
  print_int32u "int32# -> int32#, ~42" i

let () =
  let i = lognot_BtoU (-100l) in
  print_int32u "int32 -> int32#, ~(-100)" i

let () =
  let f = lognot_UtoB #255l in
  print_int32 "int32# -> int32, ~255" f

let () =
  let f = lognot_BUtoU 1024l in
  print_int32u "(int32[@unboxed]) -> int32#, ~1024" f

let () =
  let f = lognot_UtoBU (-#1726l) in
  print_int32 "int32# -> (int32[@unboxed]), ~(-1726)" f

(* If there are more than 5 args, you get an array in bytecode *)
external sum_7 :
  int32# -> int32 -> int32# -> int32 ->
  int32# -> int32 -> int32# -> int32# =
  "sum_7_bytecode" "sum_7_UBUBUBUtoU"

let _ =
  let f =
    sum_7
      #1l 2l #3l 4l
      #5l 6l #7l
  in
  print_int32u "Function of 7 args, 1+2+3+4+5+6+7" f
