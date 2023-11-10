(* TEST
   flags = "-extension layouts_alpha"
   * expect
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

let test s f = Format.printf "%s: %f\n" s (Float_u.to_float f); Format.print_flush ()

[%%expect{|
module Float_u :
  sig
    external to_float : float# -> (float [@local_opt]) = "%box_float"
    external of_float : (float [@local_opt]) -> float# = "%unbox_float"
    val neg : float# -> float#
    val add : float# -> float# -> float#
    val sub : float# -> float# -> float#
    val mul : float# -> float# -> float#
    val div : float# -> float# -> float#
    val fma : float# -> float# -> float# -> float#
    val rem : float# -> float# -> float#
    val succ : float# -> float#
    val pred : float# -> float#
    val abs : float# -> float#
    val is_finite : float# -> bool
    val is_infinite : float# -> bool
    val is_nan : float# -> bool
    val is_integer : float# -> bool
    val of_int : int -> float#
    val to_int : float# -> int
    val of_string : string -> float#
    val to_string : float# -> string
    type fpclass =
      fpclass =
        FP_normal
      | FP_subnormal
      | FP_zero
      | FP_infinite
      | FP_nan
    val classify_float : float# -> fpclass
    val pow : float# -> float# -> float#
    val sqrt : float# -> float#
    val cbrt : float# -> float#
    val exp : float# -> float#
    val exp2 : float# -> float#
    val log : float# -> float#
    val log10 : float# -> float#
    val log2 : float# -> float#
    val expm1 : float# -> float#
    val log1p : float# -> float#
    val cos : float# -> float#
    val sin : float# -> float#
    val tan : float# -> float#
    val acos : float# -> float#
    val asin : float# -> float#
    val atan : float# -> float#
    val atan2 : float# -> float# -> float#
    val hypot : float# -> float# -> float#
    val cosh : float# -> float#
    val sinh : float# -> float#
    val tanh : float# -> float#
    val acosh : float# -> float#
    val asinh : float# -> float#
    val atanh : float# -> float#
    val erf : float# -> float#
    val erfc : float# -> float#
    val trunc : float# -> float#
    val round : float# -> float#
    val ceil : float# -> float#
    val floor : float# -> float#
    val next_after : float# -> float# -> float#
    val copy_sign : float# -> float# -> float#
    val sign_bit : float# -> bool
    val ldexp : float# -> int -> float#
    type t = float#
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val min : t -> t -> t
    val max : float# -> float# -> float#
    val min_num : t -> t -> t
    val max_num : t -> t -> t
    val ( + ) : float# -> float# -> float#
    val ( - ) : float# -> float# -> float#
    val ( * ) : float# -> float# -> float#
    val ( / ) : float# -> float# -> float#
    val ( ** ) : float# -> float# -> float#
    val ( > ) : t -> t -> bool
  end
val test : string -> float# -> unit = <fun>
|}]

(*****************************************)
(* Expressions *)

let () = test "e" #2.718281828459045

[%%expect{|
e: 2.718282
|}]

let () = test "negative_one_half" (-#0.5)
[%%expect{|
negative_one_half: -0.500000
|}]

let () = test "negative_one_half" (- #0.5)
[%%expect{|
negative_one_half: -0.500000
|}]

let () = test "negative_one_half" (-.#0.5)
[%%expect{|
negative_one_half: -0.500000
|}]

let () = test "negative_one_half" (-. #0.5)
[%%expect{|
negative_one_half: -0.500000
|}]

let () = test "positive_one_dot" (+#1.)
[%%expect{|
positive_one_dot: 1.000000
|}]

let () = test "positive_one_dot" (+ #1.)
[%%expect{|
positive_one_dot: 1.000000
|}]

let () = test "positive_one_dot" (+.#1.)
[%%expect{|
positive_one_dot: 1.000000
|}]

let () = test "positive_one_dot" (+. #1.)
[%%expect{|
positive_one_dot: 1.000000
|}]

let () = test "one_billion" (#1e9)
[%%expect{|
one_billion: 1000000000.000000
|}]

let zero = #0n
[%%expect{|
Line 1, characters 11-14:
1 | let zero = #0n
               ^^^
Error: Unboxed int literals aren't supported yet.
|}]

let positive_one = +#1l
[%%expect{|
Line 1, characters 19-23:
1 | let positive_one = +#1l
                       ^^^^
Error: Unboxed int literals aren't supported yet.
|}]

let positive_one = + #1l
[%%expect{|
Line 1, characters 19-24:
1 | let positive_one = + #1l
                       ^^^^^
Error: Unboxed int literals aren't supported yet.
|}]

let negative_one = -#1L
[%%expect{|
Line 1, characters 19-23:
1 | let negative_one = -#1L
                       ^^^^
Error: Unboxed int literals aren't supported yet.
|}]

let negative_one = - #1L
[%%expect{|
Line 1, characters 19-24:
1 | let negative_one = - #1L
                       ^^^^^
Error: Unboxed int literals aren't supported yet.
|}]

let two_fifty_five_in_hex = #0xFFn
[%%expect{|
Line 1, characters 28-34:
1 | let two_fifty_five_in_hex = #0xFFn
                                ^^^^^^
Error: Unboxed int literals aren't supported yet.
|}]

let twenty_five_in_octal = #0o31l
[%%expect{|
Line 1, characters 27-33:
1 | let twenty_five_in_octal = #0o31l
                               ^^^^^^
Error: Unboxed int literals aren't supported yet.
|}]

let forty_two_in_binary = #0b101010L
[%%expect{|
Line 1, characters 26-36:
1 | let forty_two_in_binary = #0b101010L
                              ^^^^^^^^^^
Error: Unboxed int literals aren't supported yet.
|}]

let () = test "one_twenty_seven_point_two_five_in_floating_hex" (#0x7f.4)
[%%expect{|
one_twenty_seven_point_two_five_in_floating_hex: 127.250000
|}]

let () = test "five_point_three_seven_five_in_floating_hexponent" (#0xa.cp-1)
[%%expect{|
five_point_three_seven_five_in_floating_hexponent: 5.375000
|}]

let () = test "unknown_floating_point_suffix" (#0.P)
[%%expect{|
Line 1, characters 46-52:
1 | let () = test "unknown_floating_point_suffix" (#0.P)
                                                  ^^^^^^
Error: Unknown modifier 'P' for literal #0.P
|}]

(*****************************************)
(* Patterns *)

let f x =
  match x with
  | #4L -> `Four
  | #5L -> `Five
  | _ -> `Other
;;

f #4L;;
[%%expect {|
Line 3, characters 4-7:
3 |   | #4L -> `Four
        ^^^
Error: Unboxed int literals aren't supported yet.
|}];;

let f x =
  match x with
  | #4. -> `Four
  | #5. -> `Five
  | _ -> `Other
;;

f #5.;;
[%%expect {|
val f : float# -> [> `Five | `Four | `Other ] = <fun>
- : [> `Five | `Four | `Other ] = `Five
|}];;

let f x =
  match x with
  | #4. -> #0.
  | #5. -> #1.
  | x ->  x
;;

test "result" (f #7.);;
[%%expect {|
val f : float# -> float# = <fun>
result: 7.000000
- : unit = ()
|}];;

(*****************************************)
(* Lexing edge cases *)

(* Unboxed literals at the beginning of the line aren't directives. *)
let f (_ : float#) _ = ();;
let () = f
#2.
#2L
;;

let () = f
#2. #2.
;;

[%%expect{|
val f : float# -> 'a -> unit = <fun>
Line 4, characters 0-3:
4 | #2L
    ^^^
Error: Unboxed int literals aren't supported yet.
|}];;

let f _ _ = ();;
let () = f
(* This lexes as a directive. #2 is not a valid unboxed int literal
   anyway, as it lacks a suffix.
*)
#2 "literals.ml"
()
()
;;

[%%expect{|
val f : 'a -> 'b -> unit = <fun>
|}];;
