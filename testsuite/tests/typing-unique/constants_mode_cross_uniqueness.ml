(* TEST
   flags += "-extension unique";
   expect;
*)

let mode_cross_int : int @ aliased -> int @ unique = function
  | 1 -> 1
  | x -> x
[%%expect{|
val mode_cross_int : int -> int @ unique = <fun>
|}]

let mode_cross_char : char @ aliased -> char @ unique = function
  | 'a' -> 'a'
  | x -> x
[%%expect{|
val mode_cross_char : char -> char @ unique = <fun>
|}]

let mode_cross_string : string @ aliased -> string @ unique = function
  | "a" -> "a"
  | x -> x
[%%expect{|
Line 3, characters 9-10:
3 |   | x -> x
             ^
Error: This value is "aliased" but expected to be "unique".
|}]

let mode_cross_float : float @ aliased -> float @ unique = function
  | 3.14 -> 3.14
  | x -> x
[%%expect{|
Line 3, characters 9-10:
3 |   | x -> x
             ^
Error: This value is "aliased" but expected to be "unique".
|}]

let mode_cross_float32 : float32 @ aliased -> float32 @ unique = function
  | 3.14s -> 3.14s
  | x -> x
[%%expect{|
Line 3, characters 9-10:
3 |   | x -> x
             ^
Error: This value is "aliased" but expected to be "unique".
|}]

let mode_cross_unboxed_float : float# @ aliased -> float# @ unique = function
  | #3.14 -> #3.14
  | x -> x
[%%expect{|
val mode_cross_unboxed_float : float# -> float# @ unique = <fun>
|}]

let mode_cross_unboxed_float32 : float32# @ aliased -> float32# @ unique = function
  | #3.14s -> #3.14s
  | x -> x
[%%expect{|
Line 1:
Error: float32 literal patterns are not supported.
|}]

let mode_cross_int32 : int32 @ aliased -> int32 @ unique = function
  | 1l -> 1l
  | x -> x
[%%expect{|
Line 3, characters 9-10:
3 |   | x -> x
             ^
Error: This value is "aliased" but expected to be "unique".
|}]

let mode_cross_int64 : int64 @ aliased -> int64 @ unique = function
  | 1L -> 1L
  | x -> x
[%%expect{|
Line 3, characters 9-10:
3 |   | x -> x
             ^
Error: This value is "aliased" but expected to be "unique".
|}]

let mode_cross_nativeint : nativeint @ aliased -> nativeint @ unique = function
  | 1n -> 1n
  | x -> x
[%%expect{|
Line 3, characters 9-10:
3 |   | x -> x
             ^
Error: This value is "aliased" but expected to be "unique".
|}]

let mode_cross_unboxed_int32 : int32# @ aliased -> int32# @ unique = function
  | #1l -> #1l
  | x -> x
[%%expect{|
val mode_cross_unboxed_int32 : int32# -> int32# @ unique = <fun>
|}]

let mode_cross_unboxed_int64 : int64# @ aliased -> int64# @ unique = function
  | #1L -> #1L
  | x -> x
[%%expect{|
val mode_cross_unboxed_int64 : int64# -> int64# @ unique = <fun>
|}]

let mode_cross_unboxed_nativeint : nativeint# @ aliased -> nativeint# @ unique = function
  | #1n -> #1n
  | x -> x
[%%expect{|
val mode_cross_unboxed_nativeint : nativeint# -> nativeint# @ unique = <fun>
|}]
