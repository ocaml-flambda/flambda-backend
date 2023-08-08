(* TEST
   modules = "c_functions.c"
   flags = "-extension layouts_alpha"
*)

(* This file tests using external C functions with nativeint#. *)

external to_nativeint : nativeint# -> (nativeint[@local_opt]) = "%box_nativeint"
external of_nativeint : (nativeint[@local_opt]) -> nativeint# =
  "%unbox_nativeint"

let print_nativeintu s f = Printf.printf "%s: %nd\n" s (to_nativeint f)
let print_nativeint s f = Printf.printf "%s: %nd\n" s f

(* Various combinations of arguments nativeint, nativeint [@unboxed], and
   nativeint# *)
external lognot_UtoU : nativeint# -> nativeint# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_BtoU : nativeint -> nativeint# =
  "lognot_bytecode" "lognot_BtoU"
external lognot_UtoB : nativeint# -> nativeint =
  "lognot_bytecode" "lognot_UtoB"
external lognot_BUtoU : (nativeint[@unboxed]) -> nativeint# =
  "lognot_bytecode" "lognot_UtoU"
external lognot_UtoBU : nativeint# -> (nativeint[@unboxed]) =
  "lognot_bytecode" "lognot_UtoU"

let () =
  let i = lognot_UtoU (of_nativeint 42n) in
  print_nativeintu "nativeint# -> nativeint#, ~42" i

let () =
  let i = lognot_BtoU (-100n) in
  print_nativeintu "nativeint -> nativeint#, ~(-100)" i

let () =
  let f = lognot_UtoB (of_nativeint 255n) in
  print_nativeint "nativeint# -> nativeint, ~255" f

let () =
  let f = lognot_BUtoU 1024n in
  print_nativeintu "(nativeint[@unboxed]) -> nativeint#, ~1024" f

let () =
  let f = lognot_UtoBU (of_nativeint (-1726n)) in
  print_nativeint "nativeint# -> (nativeint[@unboxed]), ~(-1726)" f

(* If there are more than 5 args, you get an array in bytecode *)
external sum_7 :
  nativeint# -> nativeint -> nativeint# -> nativeint ->
  nativeint# -> nativeint -> nativeint# -> nativeint# =
  "sum_7_bytecode" "sum_7_UBUBUBUtoU"

let _ =
  let f =
    sum_7
      (of_nativeint 1n) 2n (of_nativeint 3n) 4n
      (of_nativeint 5n) 6n (of_nativeint 7n)
  in
  print_nativeintu "Function of 7 args, 1+2+3+4+5+6+7" f


