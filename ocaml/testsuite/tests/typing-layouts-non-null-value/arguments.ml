(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

module Fake_or_null : sig
  type ('a : non_null_value) t : value

  val none : 'a t
  val some : 'a -> 'a t
end = struct
  type ('a : non_null_value) t = 'a option

  let none = None
  let some x = Some x
end
;;

[%%expect{|
module Fake_or_null :
  sig
    type ('a : non_null_value) t : value
    val none : ('a : non_null_value). 'a t
    val some : ('a : non_null_value). 'a -> 'a t
  end
|}]

let _ = Fake_or_null.some (Fake_or_null.none)
;;

[%%expect{|
Line 1, characters 26-45:
1 | let _ = Fake_or_null.some (Fake_or_null.none)
                              ^^^^^^^^^^^^^^^^^^^
Error: This expression has type 'a Fake_or_null.t
       but an expression was expected of type ('b : non_null_value)
       The layout of 'a Fake_or_null.t is value, because
         of the definition of t at line 2, characters 2-38.
       But the layout of 'a Fake_or_null.t must be a sublayout of non_null_value, because
         of the definition of some at line 5, characters 2-23.
|}]


(* Built-in containers accept nullable values: *)

let _ = [ Fake_or_null.none ]

let _ = [| Fake_or_null.some 3 |]

let _ = [: Fake_or_null.some "test " :]

let _ = Some (Fake_or_null.some 4.2)

let _ = lazy (Fake_or_null.none)
;;

[%%expect{|
- : 'a Fake_or_null.t list = [<abstr>]
- : int Fake_or_null.t array = [|<abstr>|]
- : string Fake_or_null.t iarray = [:<abstr>:]
- : float Fake_or_null.t option = Some <abstr>
- : 'a Fake_or_null.t lazy_t = lazy <abstr>
|}]

module M1 : sig
  type 'a t

  val mk : 'a -> 'a t
end = struct
  type 'a t = 'a

  let mk x = x
end

(* CR layouts v3.0: abstract types and type parameters to
   abstract types should default to non-null: *)

let _ = Fake_or_null.some (M1.mk 2)
;;
[%%expect{|
module M1 : sig type 'a t val mk : 'a -> 'a t end
Line 14, characters 26-35:
14 | let _ = Fake_or_null.some (M1.mk 2)
                               ^^^^^^^^^
Error: This expression has type int M1.t
       but an expression was expected of type ('a : non_null_value)
       The layout of int M1.t is value, because
         of the definition of t at line 2, characters 2-11.
       But the layout of int M1.t must be a sublayout of non_null_value, because
         of the definition of some at line 5, characters 2-23.
|}]

let _ = M1.mk (Fake_or_null.some 5)
;;

[%%expect{|
- : int Fake_or_null.t M1.t = <abstr>
|}]

let my_id1 x = x
let my_id2 (x : 'a) = x
let my_id3 : 'a . 'a -> 'a = fun x -> x
let my_id4 (type a) (x : a) = x
;;

[%%expect{|
val my_id1 : 'a -> 'a = <fun>
val my_id2 : 'a -> 'a = <fun>
val my_id3 : 'a -> 'a = <fun>
val my_id4 : 'a -> 'a = <fun>
|}]

(* By default, type variables in functions are nullable: *)

let _ = my_id1 (Fake_or_null.some 1)

let _ = my_id2 (Fake_or_null.some 2)

let _ = my_id3 (Fake_or_null.some 3)

let _ = my_id4 (Fake_or_null.some 4)
;;

[%%expect{|
- : int Fake_or_null.t = <abstr>
- : int Fake_or_null.t = <abstr>
- : int Fake_or_null.t = <abstr>
- : int Fake_or_null.t = <abstr>
|}]
