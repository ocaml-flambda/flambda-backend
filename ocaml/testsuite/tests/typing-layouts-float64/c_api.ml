(* TEST
   modules = "stubs.c"
   * flambda2
   reference = "${test_source_directory}/c_api.reference"
   ** native
     flags = "-extension layouts_alpha"
   ** bytecode
     flags = "-extension layouts_alpha"
   ** native
     flags = "-extension layouts_beta"
   ** bytecode
     flags = "-extension layouts_beta"
*)

(* mshinwell: This test is now only run with flambda2, as the corresponding
   ocamltest predicate is reliable for testing whether this is an
   flambda-backend build. *)

(* This file tests using external C functions with float#. *)

external to_float : float# -> (float[@local_opt]) = "%box_float"
external of_float : (float[@local_opt]) -> float# = "%unbox_float"

let print_floatu s f = Printf.printf "%s: %.2f\n" s (to_float f)
let print_float s f = Printf.printf "%s: %.2f\n" s f

(* Various combinations of arguments float, float [@unboxed], and float# *)
external sin_U_U : float# -> float# = "sin_byte" "sin_U_U"
external sin_B_U : float -> float# = "sin_byte" "sin_B_U"
external sin_U_B : float# -> float = "sin_byte" "sin_U_B"

external sin_BU_U : (float[@unboxed]) -> float# = "sin_byte" "sin_U_U"
external sin_U_BU : float# -> (float[@unboxed]) = "sin_byte" "sin_U_U"

let sin_two =
  let f = sin_U_U (of_float 2.) in
  print_floatu "Test U -> U, sin two" f

let sin_three =
  let f = sin_B_U 3. in
  print_floatu "Test B -> U, sin three" f

let sin_four =
  let f = sin_U_B (of_float 4.) in
  print_float "Test U -> B, sin four" f

let sin_five =
  let f = sin_BU_U 5. in
  print_floatu "Test (B[@unboxed]) -> U, sin five" f

let sin_six =
  let f = sin_U_BU (of_float 6.) in
  print_float "Test U -> (B[@unboxed]), sin six" f

(* If there are more than 5 args, you get an array in the bytecode version,
   which is fine since the floats are boxed for bytecode. *)
external sum_7 :
  float# -> float -> float# -> float -> float# -> float -> float# -> float# =
  "sum_7_byte" "sum_7"

let sum_of_one_to_seven =
  let f =
    sum_7 (of_float 1.) 2. (of_float 3.) 4. (of_float 5.) 6. (of_float 7.)
  in
  print_floatu "Function with many args, sum_of_one_to_seven" f

(* Non-inlined eta expansion *)
let[@inline never] sin_U_U' x = sin_U_U x

let sin_seven =
  let f = sin_U_U' (of_float 7.) in
  print_floatu "Test U -> U eta expansion, sin seven" f
