(* TEST
 expect;
*)

type ('a, 'b) eq = Refl : ('a, 'a) eq
type empty = (int, string) eq

let f = function `Foo (_ : empty) -> .
[%%expect{|
type ('a, 'b) eq = Refl : ('a, 'a) eq
type empty = (int, string) eq
val f : ('a : value_or_null). [< `Foo of empty ] -> 'a = <fun>
|}]
