(* TEST
   * flambda2
   flags = "-extension layouts_alpha"
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

let test s f = Format.printf "%s: %f\n%!" s (Float_u.to_float f)

(*****************************************)
(* Expressions *)

let () = test "e" #2.718281828459045
let () = test "negative_one_half" (-#0.5)
let () = test "negative_one_half" (- #0.5)
let () = test "negative_one_half" (-.#0.5)
let () = test "negative_one_half" (-. #0.5)
let () = test "positive_one_dot" (+#1.)
let () = test "positive_one_dot" (+ #1.)
let () = test "positive_one_dot" (+.#1.)
let () = test "positive_one_dot" (+. #1.)
let () = test "one_billion" (#1e9)
let () = test "one_twenty_seven_point_two_five_in_floating_hex" (#0x7f.4)
let () = test "five_point_three_seven_five_in_floating_hexponent" (#0xa.cp-1)

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

test "result" (f #7.);;

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

test "larger match result" (f #7.);;
