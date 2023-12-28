(* TEST
   flags = "-extension layouts"
   * expect
*)

(*****************************************)
(* Prelude: Functions on unboxed floats. *)

module Float_u = Stdlib__Float_u

let test s f = Format.printf "%s: %f\n" s (Float_u.to_float f); Format.print_flush ()

[%%expect{|
module Float_u = Stdlib__Float_u
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

let f x =
  match x with
  | #4. -> #0.
  | #5. -> #1.
;;

test "result" (f #7.);;
(* This is here to test the [partial-match] warning *)
[%%expect {|
Lines 2-4, characters 2-14:
2 | ..match x with
3 |   | #4. -> #0.
4 |   | #5. -> #1.
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
#0.

val f : float# -> float# = <fun>
Exception: Match_failure ("", 2, 2).
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
