(* TEST
 flambda2;
 expect;
*)

(* This test just shows that you can't avoid the maturity checks by hiding an
   experimental layout under a product. *)
type t : void & value

[%%expect{|
type t : void & value
|}]
