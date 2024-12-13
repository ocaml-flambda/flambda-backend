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
