(* TEST
 {
   flags = "-extension layouts_alpha -I +stdlib_upstream_compatible";
   expect;
 }{
   flags = "-extension layouts_beta -I +stdlib_upstream_compatible";
   expect;
 }{
   flags = "-I +stdlib_upstream_compatible";
   expect;
 }
*)

open Stdlib_upstream_compatible

(* Test 1: mixed module with unboxed primitives *)
module M_1 = struct
  let negative_two = "-2"
  let negative_one = -#1.0
  let zero = #0.0
  let one = 1
  let two = 2
  let three = 3.0
  let four = #4L
  let five = #5l
  let six = #6n
  let seven = "7"
end

[%%expect{|
module M_1 :
  sig
    val negative_two : string
    val negative_one : float#
    val zero : float#
    val one : int
    val two : int
    val three : float
    val four : int64#
    val five : int32#
    val six : nativeint#
    val seven : string
  end
|}]

let _ = M_1.negative_two
let _ = Float_u.to_float M_1.negative_one
let _ = Float_u.to_float M_1.zero
let _ = M_1.one
let _ = M_1.two
let _ = M_1.three
let _ = Int64_u.to_int64 M_1.four
let _ = Int32_u.to_int32 M_1.five
let _ = Nativeint_u.to_nativeint M_1.six
let _ = M_1.seven

[%%expect{|
- : string = "-2"
- : float = -1.
- : float = 0.
- : int = 1
- : int = 2
- : float = 3.
- : int64 = 4L
- : int32 = 5l
- : nativeint = 6n
- : string = "7"
|}]


(* Test 2: mixed module with unboxed products *)
module M_2 = struct
  let f x = x + 1
  let number_tree = #(
    #1.0, #(#0.1, #0.2, #3L), #10l, #5n, #(#(#0.5, #0.75), #(#1n, #0.0))
  )
  let g x = x - 1
  let number_tree_2 = #(#(#(#12l, #30l), #9n), #10n)
end

[%%expect{|
module M_2 :
  sig
    val f : int -> int
    val number_tree :
      #(float# * #(float# * float# * int64#) * int32# * nativeint# *
        #(#(float# * float#) * #(nativeint# * float#)))
    val g : int -> int
    val number_tree_2 : #(#(#(int32# * int32#) * nativeint#) * nativeint#)
  end
|}]

let _ = M_2.number_tree
[%%expect{|
Line 1, characters 0-23:
1 | let _ = M_2.number_tree
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: Types of unnamed expressions must have layout value when using
       the toplevel, but this expression has layout
       "float64 & (float64 & float64 & bits64) & bits32 & word & ((float64 & float64) &
                                                                (word & float64))".
|}]
let _ = M_2.number_tree_2
[%%expect{|
Line 1, characters 0-25:
1 | let _ = M_2.number_tree_2
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Types of unnamed expressions must have layout value when using
       the toplevel, but this expression has layout
       "((bits32 & bits32) & word) & word".
|}]

(* Test 3: module coercions *)
module type S_3 = sig
  val zero : float#
  val name : string
end

module M_3 = struct
  let negative_one = -#1.0
  let two = #2.0
  let name = "M_3"
  let one = #1.0
  let zero = #0.0
end

module M_3_1 = struct
  let name = "M_3"
  let zero = 0.0
end

module N_3 : S_3 = M_3

[%%expect{|
module type S_3 = sig val zero : float# val name : string end
module M_3 :
  sig
    val negative_one : float#
    val two : float#
    val name : string
    val one : float#
    val zero : float#
  end
module M_3_1 : sig val name : string val zero : float end
module N_3 : S_3
|}]

(* Test 3.1: module does not match signature (float is not float#) *)
module N_3_1 : S_3 = M_3_1
[%%expect{|
Line 1, characters 21-26:
1 | module N_3_1 : S_3 = M_3_1
                         ^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig val name : string val zero : float end
       is not included in
         S_3
       Values do not match:
         val zero : float
       is not included in
         val zero : float#
       The type "float" is not compatible with the type "float#"
|}]

let _ = N_3.name
let _ = Float_u.of_float N_3.zero
let _ = Float_u.of_float N_3.one

[%%expect{|
- : string = "M_3"
- : float = 0.0
Line 3, characters 25-32:
3 | let _ = Float_u.of_float N_3.one
                             ^^^^^^^
Error: Unbound value "N_3.one"
|}]
