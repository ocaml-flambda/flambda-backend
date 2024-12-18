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
val f : t -> t -> bool = <fun>
|}]

(* CR layouts v7.2: The below error is not as good as it could be. The example
   case is #{y=A; _ }, but should be #{y=A; x=B}. Normal boxed records get the
   nicer pattern. See the corresponding CR in [Parmatch.discr_pat].
*)
let g t t' =
  match #{ x = t; y = t' } with
  | #{ x = A; _ } -> true
  | #{ y = B; _ } -> false
[%%expect{|
Lines 2-4, characters 2-26:
2 | ..match #{ x = t; y = t' } with
3 |   | #{ x = A; _ } -> true
4 |   | #{ y = B; _ } -> false
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
#{y=A; x=B}

val g : t -> t -> bool = <fun>
|}]
