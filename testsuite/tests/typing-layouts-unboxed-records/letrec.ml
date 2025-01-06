(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta";
 {
   expect;
 }
*)

type t : value = #{ t : t }
let rec t = #{ t = t }
[%%expect{|
type t = #{ t : t; }
Line 2, characters 12-22:
2 | let rec t = #{ t = t }
                ^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
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

(* The below is adapted from [testsuite/tests/letrec-check/unboxed.ml].

   CR layouts v7.2: once unboxed records are in stable, fold this test back into
   the original or move it to [typing-layouts-products]. *)

type t = #{x: int64}
let rec x = #{x = y} and y = 3L;;
[%%expect{|
type t = #{ x : int64; }
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
