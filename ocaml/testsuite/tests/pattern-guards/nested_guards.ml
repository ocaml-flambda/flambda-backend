(* TEST
   flags = "-extension pattern_guards"
   * expect *)

(* Tests behavior of nested pattern guards, i.e. pattern guards whose cases have
   patterns that are themselves guarded. *)

(* Nested single-way pattern guards. *)

let nested_singleway f g h ~default = function
  | Some x
      when f x match Some y
      when g y match Some z
      when h z match Some a ->
      a
  | _ -> default
;;
[%%expect{|
val nested_singleway :
  ('a -> 'b option) ->
  ('b -> 'c option) -> ('c -> 'd option) -> default:'d -> 'a option -> 'd =
  <fun>
|}];;

let collatz = function
  | 1 -> None
  | n -> if n mod 2 = 0 then Some (n / 2) else Some (3 * n + 1)
;;
[%%expect{|
val collatz : int -> int option = <fun>
|}];;

nested_singleway collatz collatz collatz ~default:~-1 None;;
[%%expect{|
- : int = -1
|}];;
nested_singleway collatz collatz collatz ~default:~-1 (Some 1);;
[%%expect{|
- : int = -1
|}];;
nested_singleway collatz collatz collatz ~default:~-1 (Some 2);;
[%%expect{|
- : int = -1
|}];;
nested_singleway collatz collatz collatz ~default:~-1 (Some 3);;
[%%expect{|
- : int = 16
|}];;
nested_singleway collatz collatz collatz ~default:~-1 (Some 4);;
[%%expect{|
- : int = -1
|}];;
nested_singleway collatz collatz collatz ~default:~-1 (Some 8);;
[%%expect{|
- : int = 1
|}];;

(* Nested multiway pattern guards. *)

let nested_multiway f g h = function
  | Some x when f x match (
      | "foo" when h x -> "foo1"
      | "bar" when g x match (
          | [] -> "bar empty"
          | [ y ] -> "bar singleton " ^ y
        )
    )
  | _ -> "not found"
;;
[%%expect{|
val nested_multiway :
  ('a -> string) ->
  ('a -> string list) -> ('a -> bool) -> 'a option -> string = <fun>
|}];;

let f = function
  | 0 | 1 -> "foo"
  | 10 | 100 | 1000 -> "bar"
  | _ -> "neither"
;;
[%%expect{|
val f : int -> string = <fun>
|}];;

let g = function
  | 10 -> []
  | 100 -> [ "one" ]
  | _ -> [ "more"; "than"; "one" ]
;;
[%%expect{|
val g : int -> string list = <fun>
|}];;

let h x = x = 1;;
[%%expect{|
val h : int -> bool = <fun>
|}];;

nested_multiway f g h None;;
[%%expect{|
- : string = "not found"
|}];;
nested_multiway f g h (Some 0);;
[%%expect{|
- : string = "not found"
|}];;
nested_multiway f g h (Some 1);;
[%%expect{|
- : string = "foo1"
|}];;
nested_multiway f g h (Some 10);;
[%%expect{|
- : string = "bar empty"
|}];;
nested_multiway f g h (Some 100);;
[%%expect{|
- : string = "bar singleton one"
|}];;
nested_multiway f g h (Some 1000);;
[%%expect{|
- : string = "not found"
|}];;
