(* TEST
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta";
 expect;
*)

(*****************************************)
(* Prelude: Functions on unboxed floats. *)

module Float_u = Stdlib_upstream_compatible.Float_u
module Int32_u = Stdlib_upstream_compatible.Int32_u
module Int64_u = Stdlib_upstream_compatible.Int64_u
module Nativeint_u = Stdlib_upstream_compatible.Nativeint_u

let test_float s f =
  Format.printf "%s: %f\n" s (Float_u.to_float f); Format.print_flush ()
let test_int32 s f =
  Format.printf "%s: %ld\n" s (Int32_u.to_int32 f); Format.print_flush ()
let test_int64 s f =
  Format.printf "%s: %Ld\n" s (Int64_u.to_int64 f); Format.print_flush ()
let test_nativeint s f =
  Format.printf "%s: %s\n" s (Nativeint_u.to_string f); Format.print_flush ()

[%%expect{|
module Float_u = Stdlib_upstream_compatible.Float_u
module Int32_u = Stdlib_upstream_compatible.Int32_u
module Int64_u = Stdlib_upstream_compatible.Int64_u
module Nativeint_u = Stdlib_upstream_compatible.Nativeint_u
val test_float : string -> Float_u.t -> unit = <fun>
val test_int32 : string -> Int32_u.t -> unit = <fun>
val test_int64 : string -> Int64_u.t -> unit = <fun>
val test_nativeint : string -> Nativeint_u.t -> unit = <fun>
|}]

(*****************************************)
(* Expressions *)

let () = test_float "e" #2.718281828459045

[%%expect{|
e: 2.718282
|}]

let () = test_float "negative_one_half" (-#0.5)
[%%expect{|
negative_one_half: -0.500000
|}]

let () = test_float "negative_one_half" (- #0.5)
[%%expect{|
negative_one_half: -0.500000
|}]

let () = test_float "negative_one_half" (-.#0.5)
[%%expect{|
negative_one_half: -0.500000
|}]

let () = test_float "negative_one_half" (-. #0.5)
[%%expect{|
negative_one_half: -0.500000
|}]

let () = test_float "positive_one_dot" (+#1.)
[%%expect{|
positive_one_dot: 1.000000
|}]

let () = test_float "positive_one_dot" (+ #1.)
[%%expect{|
positive_one_dot: 1.000000
|}]

let () = test_float "positive_one_dot" (+.#1.)
[%%expect{|
positive_one_dot: 1.000000
|}]

let () = test_float "positive_one_dot" (+. #1.)
[%%expect{|
positive_one_dot: 1.000000
|}]

let () = test_float "one_billion" (#1e9)
[%%expect{|
one_billion: 1000000000.000000
|}]
let () = test_nativeint "zero" (#0n)
[%%expect{|
zero: 0
|}]
let () = test_int32 "positive_one" (+#1l)
[%%expect{|
positive_one: 1
|}]
let () = test_int32 "positive_one" (+ #1l)
[%%expect{|
positive_one: 1
|}]
let () = test_int64 "negative_one" (-#1L)
[%%expect{|
negative_one: -1
|}]
let () = test_int64 "negative_one" (- #1L)
[%%expect{|
negative_one: -1
|}]
let () = test_nativeint "two_fifty_five_in_hex" (#0xFFn)
[%%expect{|
two_fifty_five_in_hex: 255
|}]
let () = test_int32 "twenty_five_in_octal" (#0o31l)
[%%expect{|
twenty_five_in_octal: 25
|}]
let () = test_int64 "forty_two_in_binary" (#0b101010L)
[%%expect{|
forty_two_in_binary: 42
|}]

let () = test_float "one_twenty_seven_point_two_five_in_floating_hex" (#0x7f.4)
[%%expect{|
one_twenty_seven_point_two_five_in_floating_hex: 127.250000
|}]

let () = test_float "five_point_three_seven_five_in_floating_hexponent" (#0xa.cp-1)
[%%expect{|
five_point_three_seven_five_in_floating_hexponent: 5.375000
|}]

let () = test_float "unknown_floating_point_suffix" (#0.P)
[%%expect{|
Line 1, characters 52-58:
1 | let () = test_float "unknown_floating_point_suffix" (#0.P)
                                                        ^^^^^^
Error: Unknown modifier P for literal #0.P
|}]

let () = test_int32 "unknown_int_suffix" (#0g)
[%%expect{|
Line 1, characters 41-46:
1 | let () = test_int32 "unknown_int_suffix" (#0g)
                                             ^^^^^
Error: Unknown modifier g for literal #0g
|}]


let () = test_nativeint "invalid_nativeint" (#0x10000000000000000n)
[%%expect{|
Line 1, characters 44-67:
1 | let () = test_nativeint "invalid_nativeint" (#0x10000000000000000n)
                                                ^^^^^^^^^^^^^^^^^^^^^^^
Error: Integer literal exceeds the range of representable integers of type nativeint#
|}]

let () = test_int64 "invalid_int64" (#0x10000000000000000L)
[%%expect{|
Line 1, characters 36-59:
1 | let () = test_int64 "invalid_int64" (#0x10000000000000000L)
                                        ^^^^^^^^^^^^^^^^^^^^^^^
Error: Integer literal exceeds the range of representable integers of type int64#
|}]

let () = test_int32 "invalid_int32" (#0x100000000l)
[%%expect{|
Line 1, characters 36-51:
1 | let () = test_int32 "invalid_int32" (#0x100000000l)
                                        ^^^^^^^^^^^^^^^
Error: Integer literal exceeds the range of representable integers of type int32#
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
val f : int64# -> [> `Five | `Four | `Other ] = <fun>
- : [> `Five | `Four | `Other ] = `Four
|}];;

let f x =
  match x with
  | #4L -> #0L
  | #5L -> #1L
  | x ->  x
;;

test_int64 "result" (f #7L);;
[%%expect {|
val f : int64# -> int64# = <fun>
result: 7
- : unit = ()
|}];;

let f x =
  match x with
  | #0L -> #0L
  | #1L -> #0L
  | #2L -> #0L
  | #4L -> #0L
  | #5L -> #1L
;;

test_int64 "result" (f #7L);;
(* This is here to test the [partial-match] warning *)
[%%expect {|
Lines 2-7, characters 2-14:
2 | ..match x with
3 |   | #0L -> #0L
4 |   | #1L -> #0L
5 |   | #2L -> #0L
6 |   | #4L -> #0L
7 |   | #5L -> #1L
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
#3L

val f : int64# -> int64# = <fun>
Exception: Match_failure ("", 2, 2).
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

test_float "result" (f #7.);;
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

test_float "result" (f #7.);;
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
let f1 (_ : float#) (_ : int64#) = ();;
let f2 (_ : float#) (_ : float#) = ();;
let () = f1
#2.
#2L
;;

let () = f2
#2. #2.
;;

[%%expect{|
val f1 : float# -> int64# -> unit = <fun>
val f2 : float# -> float# -> unit = <fun>
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
