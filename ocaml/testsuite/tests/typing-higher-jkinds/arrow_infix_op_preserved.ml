(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

let (=>) x y = 2 * x + y

(* Note that the language defines (=>) to be left-associative as = is first. *)
(* left-associative:  2 * (2 * 5 + 3) + 1 = 2 * 13 + 1 = 27 *)
(* right-associative: 2 * 5 + (2 * 3 + 1) = 10 + 6 + 1 = 17 *)
let t = 5 => 3 => 1

[%%expect{|
val ( => ) : int -> int -> int = <fun>
val t : int = 27
|}]
