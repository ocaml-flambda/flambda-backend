(* TEST
   * expect
*)

type t = vec128;;
[%%expect{|
Line 1, characters 9-15:
1 | type t = vec128;;
             ^^^^^^
Error: Unbound type constructor vec128
|}];;
