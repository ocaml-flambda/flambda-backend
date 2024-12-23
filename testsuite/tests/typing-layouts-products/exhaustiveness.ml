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
val f : t -> 'a -> bool = <fun>
|}]
