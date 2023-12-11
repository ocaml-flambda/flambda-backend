(* TEST
   * flambda2
   flags = "-extension layouts"
   ** native
   ** bytecode
*)

(*****************************************)
(* Prelude: Functions on unboxed floats. *)

module Float_u = struct
  include Stdlib__Float_u

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( ** ) = pow
  let ( > ) x y = (compare x y) > 0
end
module Int32_u = Stdlib__Int32_u
module Int64_u = Stdlib__Int64_u
module Nativeint_u = Stdlib__Nativeint_u

let test_float s f =
  Format.printf "%s: %f\n" s (Float_u.to_float f); Format.print_flush ()
let test_int32 s f =
  Format.printf "%s: %ld\n" s (Int32_u.to_int32 f); Format.print_flush ()
let test_int64 s f =
  Format.printf "%s: %Ld\n" s (Int64_u.to_int64 f); Format.print_flush ()
let test_nativeint s f =
  Format.printf "%s: %s\n" s (Nativeint_u.to_string f); Format.print_flush ()

(*****************************************)
(* Expressions *)

let () = test_float "e" #2.718281828459045
let () = test_float "negative_one_half" (-#0.5)
let () = test_float "negative_one_half" (- #0.5)
let () = test_float "negative_one_half" (-.#0.5)
let () = test_float "negative_one_half" (-. #0.5)
let () = test_float "positive_one_dot" (+#1.)
let () = test_float "positive_one_dot" (+ #1.)
let () = test_float "positive_one_dot" (+.#1.)
let () = test_float "positive_one_dot" (+. #1.)
let () = test_float "one_billion" (#1e9)
let () = test_float "one_twenty_seven_point_two_five_in_floating_hex" (#0x7f.4)
let () = test_float "five_point_three_seven_five_in_floating_hexponent" (#0xa.cp-1)

let () = test_nativeint "zero" (#0n)
let () = test_int32 "positive_one" (+#1l)
let () = test_int32 "positive_one" (+ #1l)
let () = test_int64 "negative_one" (-#1L)
let () = test_int64 "negative_one" (- #1L)
let () = test_nativeint "two_fifty_five_in_hex" (#0xFFn)
let () = test_int32 "twenty_five_in_octal" (#0o31l)
let () = test_int64 "forty_two_in_binary" (#0b101010L)

(*****************************************)
(* Patterns *)

let f x =
  match x with
  | #4. -> `Four
  | #5. -> `Five
  | _ -> `Other
;;

let () =
  match f #5. with
  | `Five -> ()
  | _ -> assert false;;

let f x =
  match x with
  | #4. -> #0.
  | #5. -> #1.
  | x ->  x
;;

test_float "result" (f #7.);;

let f x =
  match x with
  | #4. -> #0.
  | #5. -> #1.
  | #6. -> #2.
  | #7. -> #3.
  | #8. -> #4.
  | #9. -> #5.
  | #10. -> #6.
  | #11. -> #7.
  | x ->  x
;;

test_float "larger match result" (f #7.);;


let f x =
  match x with
  | #4L -> `Four
  | #5L -> `Five
  | _ -> `Other
;;

let () =
  match f #4L with
  | `Four -> ()
  | _ -> assert false;;

let f x =
  match x with
  | #4L -> #0L
  | #5L -> #1L
  | x ->  x
;;

test_int64 "result" (f #7L);;
