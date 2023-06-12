(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

(* CR layouts v2: This is just a hack that works whilst [float#]s get compiled
   to [float]s. We should change this soon.
*)
let box : float# -> float = fun x -> Sys.opaque_identity (Obj.magic x)
let unbox : float -> float# = fun x -> Sys.opaque_identity (Obj.magic x);;
[%%expect {|
val box : float# -> float = <fun>
val unbox : float -> float# = <fun>
|}];;

let id : float# -> float# = fun x -> x;;

box (id #4.0);;
[%%expect {|
val id : float# -> float# = <fun>
- : float = 4.
|}];;

(* CR layouts: We should actually add the numbers here when
   we support that.
*)
let add (x : float#) (y : float#) = unbox (box x +. box y);;

box (add #4.0 #5.0);;
[%%expect {|
val add : float# -> float# -> float# = <fun>
- : float = 9.
|}];;

let apply (f : float# -> float# -> float#) (x : float#) (y : float#) =
  f x y;;

box (apply add #4.0 #5.0);;
[%%expect {|
val apply : (float# -> float# -> float#) -> float# -> float# -> float# =
  <fun>
- : float = 9.
|}];;
