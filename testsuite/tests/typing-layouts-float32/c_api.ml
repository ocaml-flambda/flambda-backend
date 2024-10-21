(* TEST
 modules = "stubs.c";
 reference = "${test_source_directory}/c_api.reference";
 flambda2;
 {
   flags = "-extension small_numbers";
   native;
 }{
   flags = "-extension small_numbers";
   bytecode;
 }{
   flags = "-extension layouts_alpha -extension small_numbers";
   native;
 }{
   flags = "-extension layouts_alpha -extension small_numbers";
   bytecode;
 }{
   flags = "-extension layouts_beta -extension small_numbers";
   native;
 }{
   flags = "-extension layouts_beta -extension small_numbers";
   bytecode;
 }
*)

(* mshinwell: This test is now only run with flambda2, as the corresponding
   ocamltest predicate is reliable for testing whether this is an
   flambda-backend build. *)

(* This file tests using external C functions with float32#. *)

external to_float : float32 -> float = "%floatoffloat32"
external to_float32 : float32# -> (float32[@local_opt]) = "%box_float32"

let print_floatu s f = Printf.printf "%s: %.2f\n" s (to_float (to_float32 f))
let print_float s f = Printf.printf "%s: %.2f\n" s (to_float f)

(* Various combinations of arguments float, float [@unboxed], and float32# *)
external sin_U_U : float32# -> float32# = "sin_byte" "sin_U_U"
external sin_B_U : float32 -> float32# = "sin_byte" "sin_B_U"
external sin_U_B : float32# -> float32 = "sin_byte" "sin_U_B"

external sin_BU_U : (float32[@unboxed]) -> float32# = "sin_byte" "sin_U_U"
external sin_U_BU : float32# -> (float32[@unboxed]) = "sin_byte" "sin_U_U"

let sin_two =
  let f = sin_U_U #2.s in
  print_floatu "Test U -> U, sin two" f

let sin_three =
  let f = sin_B_U 3.s in
  print_floatu "Test B -> U, sin three" f

let sin_four =
  let f = sin_U_B #4.s in
  print_float "Test U -> B, sin four" f

let sin_five =
  let f = sin_BU_U 5.s in
  print_floatu "Test (B[@unboxed]) -> U, sin five" f

let sin_six =
  let f = sin_U_BU #6.s in
  print_float "Test U -> (B[@unboxed]), sin six" f

(* If there are more than 5 args, you get an array in the bytecode version,
   which is fine since the floats are boxed for bytecode. *)
external sum_7 :
  float32# -> float32 -> float32# -> float32 -> float32# -> float32 -> float32# -> float32# =
  "sum_7_byte" "sum_7"

let sum_of_one_to_seven =
  let f =
    sum_7 #1.s 2.s #3.s 4.s #5.s 6.s #7.s
  in
  print_floatu "Function with many args, sum_of_one_to_seven" f

(* Non-inlined eta expansion *)
let[@inline never] sin_U_U' x = sin_U_U x

let sin_seven =
  let f = sin_U_U' #7.s in
  print_floatu "Test U -> U eta expansion, sin seven" f
