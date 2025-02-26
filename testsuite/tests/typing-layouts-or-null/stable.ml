(* TEST
 flags = "-extension-universe stable";
 expect;
*)

(* CR dkalinichenko: [or_null] is not available in [stable]. *)
let x : int or_null = This 3

[%%expect{|
Line 1, characters 12-19:
1 | let x : int or_null = This 3
                ^^^^^^^
Error: Unbound type constructor "or_null"
|}]
