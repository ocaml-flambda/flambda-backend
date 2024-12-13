(* TEST
   flags = "-w +8 -extension layouts_beta";
   expect;
*)

(* This is a regression test. The example below used to give an exhaustiveness
   warning because we forgot a case in [Parmatch.simple_match]. *)

type t = A | B
type r = #{ x : t; y : t }

let f t t' =
  match #{ x = t; y = t' } with
  | #{ x = A; y = _ } -> true
  | #{ x = B; y = _ } -> false
[%%expect{|
type t = A | B
type r = #{ x : t; y : t; }
Lines 5-7, characters 2-30:
5 | ..match #{ x = t; y = t' } with
6 |   | #{ x = A; y = _ } -> true
7 |   | #{ x = B; y = _ } -> false
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
#{x=B; _ }

val f : t -> t -> bool = <fun>
|}]
