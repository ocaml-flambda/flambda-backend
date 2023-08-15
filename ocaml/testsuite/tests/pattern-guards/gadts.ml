(* TEST
   flags = "-extension pattern_guards"
   * expect *)

(* Test pattern guard inward propagation of GADT type information. *)

type ('a, 'b) eq = Eq : ('a, 'a) eq;;
[%%expect{|
type ('a, 'b) eq = Eq : ('a, 'a) eq
|}];;

let in_pattern_guard (type a b) (eq : (a, b) eq) (compare : a -> a -> int)
                     (x : a) (y : b) =
  match eq with
  | Eq when compare x y match 0 -> true
  | _ -> false
;;
[%%expect{|
val in_pattern_guard : ('a, 'b) eq -> ('a -> 'a -> int) -> 'a -> 'b -> bool =
  <fun>
|}];;

let from_pattern_guard (type a b) (eqs : (a, b) eq option list)
                       (compare : a -> a -> int) (x : a) (y : b) =
  match eqs with
  | eq_opt :: _ when eq_opt match Some Eq -> compare x y
  | _ -> 0
;;
[%%expect{|
val from_pattern_guard :
  ('a, 'b) eq option list -> ('a -> 'a -> int) -> 'a -> 'b -> int = <fun>
|}];;

