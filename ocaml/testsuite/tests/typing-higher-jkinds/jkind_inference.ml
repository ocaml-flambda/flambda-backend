(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type ('a : value => value) t = unit 'a
[%%expect {|
type ('a : value => value) t = unit 'a
|}]

type 'a t = unit 'a
[%%expect {|
Line 1, characters 12-19:
1 | type 'a t = unit 'a
                ^^^^^^^
Error: The type expression ('a : '_representable_layout_1)
         is applied as a type constructor, but it is not of a higher jkind.
|}]

type ('a, 'm) t = 'a 'm
[%%expect {|
Line 1, characters 18-23:
1 | type ('a, 'm) t = 'a 'm
                      ^^^^^
Error: The type expression ('m : '_representable_layout_2)
         is applied as a type constructor, but it is not of a higher jkind.
|}]

type ('a, 'm : value => value) t = 'a 'm
[%%expect {|
type ('a, 'm : value => value) t = 'a 'm
|}]

type ('a, 'm : immediate => immediate) t = 'a 'm
[%%expect {|
type ('a : immediate, 'm : immediate => immediate) t = 'a 'm
|}]

type ('a, 'b, 'c : (value, immediate) => value) t = ('a, 'b) 'c
[%%expect {|
type ('a, 'b : immediate, 'c : (value, immediate) => value) t = ('a, 'b) 'c
|}]

module type M = sig
  val f : ('a : value => value). unit -> unit 'a
end
[%%expect {|
module type M = sig val f : ('a : value => value). unit -> unit 'a end
|}]

module type M = sig
  val f : unit -> unit 'a
end
[%%expect {|
module type M = sig val f : ('a : value => value). unit -> unit 'a end
|}]

module type M = sig
  val f : 'a 'b -> int 'a -> list 'b
end
[%%expect {|
Line 2, characters 23-25:
2 |   val f : 'a 'b -> int 'a -> list 'b
                           ^^
Error: No consistent jkind could be inferred for 'a.
         Hint: try annotating the type variable at its binding site.
         The kind of 'a is '_representable_layout_3
           because its jkind was inferred in a type application.
         But the kind of 'a must overlap with
           (('_representable_layout_4) => '_representable_layout_5)
           because its jkind was inferred in a type application.
|}]

module type M = sig
  val f : int 'a -> list 'b -> 'a 'b
end
[%%expect {|
Line 2, characters 20-27:
2 |   val f : int 'a -> list 'b -> 'a 'b
                        ^^^^^^^
Error: The type expression ('b : (('_representable_layout_6) => '_representable_layout_7))
         cannot be applied to the arguments (list : ((value) => value)).
|}]

module type M = sig
  type (_ : any => value) s
  type (_ : value => value) t
  val f : 'a s -> 'a t
end
[%%expect {|
module type M =
  sig
    type (_ : any => value) s
    type (_ : value => value) t
    val f : ('a : any => value). 'a s -> 'a t
  end
|}]

module type M = sig
  type (_ : float64 => value) f64monad
  val f : 'm f64monad -> 'a -> 'a 'm
end
[%%expect {|
module type M =
  sig
    type (_ : float64 => value) f64monad
    val f :
      ('m : float64 => value) ('a : float64). 'm f64monad -> 'a -> 'a 'm
  end
|}]

module type M = sig
  val f : 'a -> 'a 'b
end
[%%expect {|
module type M = sig val f : 'a ('b : value => value). 'a -> 'a 'b end
|}]

module type M = sig
  type t : (((top => top) => top) => top) => any
  val f : 'a t -> unit
end
[%%expect {|
module type M =
  sig
    type t : (((top => top) => top) => top) => any
    val f : ('a : ((top => value) => top) => value). 'a t -> unit
  end
|}]

module type M = sig
  val f : 'a 'b 'c 'd -> unit 'd
end
[%%expect {|
module type M =
  sig
    val f :
      ('d : value => value) ('c : value => value) ('b : value => value) 'a.
        'a 'b 'c 'd -> unit 'd
  end
|}]

module type M = sig
  val f : 'a ('b ('c 'd)) -> unit
end
[%%expect {|
module type M =
  sig
    val f :
      ('d : value => value => value => value) 'c 'b 'a.
        'a ('b ('c 'd)) -> unit
  end
|}]

module type M = sig
  val f : 'a 'm -> 'b 'm -> 'c 'm
end

[%%expect{|
module type M =
  sig val f : ('m : value => value) 'a 'b 'c. 'a 'm -> 'b 'm -> 'c 'm end
|}]


(* Order-dependence tests:
   The inferred types should be equal up to order of arguments,
   and the type variables should have the same inferred jkinds *)

type ('f : any => value) any_to_value
module type S = sig 
  val foo1 : 'f any_to_value -> 'a 'f -> unit 
  val foo2 : 'a 'f -> 'f any_to_value -> unit
end
[%%expect{|
type ('f : any => value) any_to_value
module type S =
  sig
    val foo1 : ('f : any => value) 'a. 'f any_to_value -> 'a 'f -> unit
    val foo2 : ('f : any => value) 'a. 'a 'f -> 'f any_to_value -> unit
  end
|}]

(* This one is slightly non-obvious: 
   constraining ['f] with [(value => value) => value] and [_ => value] 
   yields ['f : top => value]. 
   Note that both constrainings happen, with 'a inferred as _ (sort var), 
   and yielding neither (value => value) nor an error.
   This is below the representable sub-lattice, as only contravariant 
   positions are non-representable. *)
type ('f : (value => value) => value) third_order
module type S = sig 
  val foo1 : 'f third_order -> 'a 'f -> unit
  val foo2 : 'a 'f -> 'f third_order -> unit
end
[%%expect{|
type ('f : (value => value) => value) third_order
module type S =
  sig
    val foo1 : ('f : top => value) 'a. 'f third_order -> 'a 'f -> unit
    val foo2 : ('f : top => value) 'a. 'a 'f -> 'f third_order -> unit
  end
|}]