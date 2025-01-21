(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 {
   expect;
 }
*)

(* This test was made to error by disallowing singleton recursive unboxed types.
   We keep it in case these are re-allowed, in which case it should error with:
   [This kind of expression is not allowed as right-hand side of "let rec"] *)
type t : value = #{ t : t }
let rec t = #{ t = t }
[%%expect{|
Line 1, characters 0-27:
1 | type t : value = #{ t : t }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "t" is recursive without boxing:
         "t" contains "t"
|}]


type t : value = { u : t }
let rec t = #{ u = t }
[%%expect{|
type t = { u : t; }
Line 2, characters 12-22:
2 | let rec t = #{ u = t }
                ^^^^^^^^^^
Error: This expression has type "t#" but an expression was expected of type "t"
|}]

type bx = { bx : ubx }
and ubx = #{ ubx : bx }
[%%expect{|
type bx = { bx : ubx; }
and ubx = #{ ubx : bx; }
|}]

let rec t = #{ ubx = { bx = t } }
[%%expect{|
val t : ubx = #{ubx = {bx = <cycle>}}
|}]

let rec t = { bx = #{ ubx = t } }
[%%expect{|
val t : bx = {bx = <cycle>}
|}]

type bx = { bx : bx2# }
and bx2 = { bx2 : bx }
[%%expect{|
type bx = { bx : bx2#; }
and bx2 = { bx2 : bx; }
|}]

let rec t = #{ bx2 = { bx = t } }
[%%expect{|
val t : bx2# = #{bx2 = {bx = <cycle>}}
|}]

let rec t = { bx = #{ bx2 = t } }
[%%expect{|
val t : bx = {bx = <cycle>}
|}]

(* The below is adapted from [testsuite/tests/letrec-check/unboxed.ml]. *)

type t = #{x: int64}
let rec x = #{x = y} and y = 3L;;
[%%expect{|
type t = #{ x : int64; }
Line 2, characters 12-20:
2 | let rec x = #{x = y} and y = 3L;;
                ^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

type t = {x: int64}
let rec x = #{x = y} and y = 3L;;
[%%expect{|
type t = { x : int64; }
Line 2, characters 12-20:
2 | let rec x = #{x = y} and y = 3L;;
                ^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

(* This test is not allowed if 'a' is unboxed, but should be accepted
   as written *)
type a = {a: b}
and b = X of a | Y

let rec a =
  {a=
    (if Sys.opaque_identity true then
       X a
     else
       Y)};;
[%%expect{|
type a = { a : b; }
and b = X of a | Y
val a : a = {a = X <cycle>}
|}];;

type a = #{ a: b }
and b = X of a | Y

let rec a =
  #{a=
    (if Sys.opaque_identity true then
       X a
     else
       Y)};;
[%%expect{|
type a = #{ a : b; }
and b = X of a | Y
Lines 5-9, characters 2-10:
5 | ..#{a=
6 |     (if Sys.opaque_identity true then
7 |        X a
8 |      else
9 |        Y)}..
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

type a = { a: b }
and b = X of a# | Y

let rec a =
  #{a=
    (if Sys.opaque_identity true then
       X a
     else
       Y)};;
[%%expect{|
type a = { a : b; }
and b = X of a# | Y
Lines 5-9, characters 2-10:
5 | ..#{a=
6 |     (if Sys.opaque_identity true then
7 |        X a
8 |      else
9 |        Y)}..
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;
