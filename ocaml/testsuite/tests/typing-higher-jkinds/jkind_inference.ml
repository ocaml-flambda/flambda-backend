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
Line 2, characters 19-25:
2 |   val f : 'a 'b -> int 'a -> list 'b
                       ^^^^^^
Error: The type expression ('a : '_representable_layout_3)
         is applied as a type constructor, but it is not of a higher jkind.
|}]

module type M = sig
  val f : int 'a -> list 'b -> 'a 'b
end
[%%expect {|
Line 2, characters 20-27:
2 |   val f : int 'a -> list 'b -> 'a 'b
                        ^^^^^^^
Error: The type expression ('b : (('_representable_layout_4) => '_representable_layout_5))
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
  type t : immediate => value
  val f : 'a 'm -> 'b 'm -> 'c 'm -> 'd 'm -> 'e 'm -> 'm
end

[%%expect{|
Line 3, characters 55-57:
3 |   val f : 'a 'm -> 'b 'm -> 'c 'm -> 'd 'm -> 'e 'm -> 'm
                                                           ^^
Error: Function return types must have a representable layout.
       The kind of 'm is
         (('_representable_layout_6) => '_representable_layout_7)
         because it was defaulted in inference.
       But the kind of 'm must overlap with any
         because argument or result of a function type.
|}]
