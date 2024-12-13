(* TEST
   flags = "-w +8";
   expect;
*)

(* This is a regression test. The example below used to give an exhaustiveness
   warning because we forgot a case in [Parmatch.simple_match]. *)

type t = A | B

let f t t' =
  match #(t,t') with
  | #(A, _) -> true
  | #(B, _) -> false
[%%expect{|
type t = A | B
Lines 4-6, characters 2-20:
4 | ..match #(t,t') with
5 |   | #(A, _) -> true
6 |   | #(B, _) -> false
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
#(B, _)

val f : t -> 'a -> bool = <fun>
|}]
