(* TEST
 flambda2;
 expect;
*)

(* This test just shows that you can't avoid the maturity checks by hiding an
   experimental layout under a product. *)
type t : void & value

[%%expect{|
Line 1, characters 9-21:
1 | type t : void & value
             ^^^^^^^^^^^^
Error: Layout void & value is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}]
