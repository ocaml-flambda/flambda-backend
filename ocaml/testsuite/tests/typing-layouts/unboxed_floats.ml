(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

(* CR layouts: These tests will change when we remove the obvious inaccuracy
   where float# is aliased to float.
*)

let id : float# -> float# = fun x -> x;;

id #4.0;;
[%%expect {|
val id : float# -> float# = <fun>
- : float# = 4.
|}];;

let add (x : float#) (y : float#) = x +. y;;

add #4.0 (#5.0);;
[%%expect {|
val add : float# -> float# -> float = <fun>
- : float = 9.
|}];;

let apply (f : float# -> float# -> float#) (x : float#) (y : float#) =
  f x y;;

apply add (#4.0) (#5.0);;
[%%expect {|
val apply : (float# -> float# -> float#) -> float# -> float# -> float# =
  <fun>
- : float# = 9.
|}];;
