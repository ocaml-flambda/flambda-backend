(* TEST
   * expect
*)

type t = float32;;
[%%expect{|
Line 1, characters 9-16:
1 | type t = float32;;
             ^^^^^^^
Error: Unbound type constructor float32
Hint: Did you mean float, float# or float32x4?
|}];;
