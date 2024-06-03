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

let _ = Some (Fake_or_null.some 42)

let _ = lazy (Fake_or_null.none)
;;

[%%expect{|
- : 'a Fake_or_null.t list = [<abstr>]
- : int Fake_or_null.t array = [|<abstr>|]
- : string Fake_or_null.t iarray = [:<abstr>:]
- : int Fake_or_null.t option = Some <abstr>
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

(* Check behavior of type arguments and unboxed annotations. *)

module M2 : sig
  type 'a t = { v : 'a } [@@unboxed]

  val box : 'a -> 'a t
  val unbox : 'a t -> 'a
end = struct
  type 'a t = { v : 'a } [@@unboxed]

  let box v = { v }
  let unbox { v } = v
end

[%%expect{|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |   type 'a t = { v : 'a } [@@unboxed]
 8 |
 9 |   let box v = { v }
10 |   let unbox { v } = v
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type 'a t = { v : 'a; } [@@unboxed]
           val box : 'a -> 'a t
           val unbox : ('a : non_null_value). 'a t -> 'a
         end
       is not included in
         sig
           type 'a t = { v : 'a; } [@@unboxed]
           val box : 'a -> 'a t
           val unbox : 'a t -> 'a
         end
       Values do not match:
         val unbox : ('a : non_null_value). 'a t -> 'a
       is not included in
         val unbox : 'a t -> 'a
       The type 'a t -> 'a is not compatible with the type 'b t -> 'b
       The layout of 'a is value, because
         of the definition of unbox at line 5, characters 2-24.
       But the layout of 'a must be a sublayout of non_null_value, because
         of the definition of unbox at line 10, characters 12-21.
|}]

module M3 : sig
  type 'a t = V of 'a [@@unboxed]

  val box : 'a -> 'a t
  val unbox : 'a t -> 'a
end = struct
  type 'a t = V of 'a [@@unboxed]

  let box v = V v
  let unbox (V v) = v
end

[%%expect{|
module M3 :
  sig
    type 'a t = V of 'a [@@unboxed]
    val box : 'a -> 'a t
    val unbox : 'a t -> 'a
  end
|}]
