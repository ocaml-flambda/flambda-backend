(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type t : value => value

[%%expect {|
type t = list
|}]


type p = int t

[%%expect {|
type t = list
|}]


module M : sig
  type p = int t
end = struct
  type p = int t
end

[%%expect {|
type t = list
|}]


module M : sig
  type 'a p = 'a t
end = struct
  type 'a p = 'a t
end

[%%expect {|
type t = list
|}]


module M : sig
  type p
end = struct
  type p = int list
end

[%%expect {|
type t = list
|}]


type p = t t

[%%expect {|
type t = list
|}]


type r' : (value => value) => value

[%%expect {|
type t = list
|}]


type ('a : value => value) r'

[%%expect {|
type ('a : ((higher))) r'
|}]


module type M = sig
  val g : ('a : value => value). 'a r -> 'a r
end

[%%expect{|
module type M = sig val g : ('a : ((higher))). 'a r -> 'a r end
|}]


module type M = sig
  type r : (value => value) => value
  type s : value => value
  val g : s r -> s r
end

[%%expect{|
module type M =
  sig
    type r : ((value) => value) => value
    type s : (value) => value
    val g : s r -> s r
  end
|}]


module type M = sig
  type r : (value => value) => value
  type s : value => value
  val g : int s r -> int s r
end

[%%expect{|
Line 4, characters 10-15:
4 |   val g : int s r -> int s r
              ^^^^^
Error: This type int s should be an instance of type ('a : ((higher)))
       The layout of int s is value, because
         of the definition of s at line 3, characters 2-25.
       But the layout of int s must be a sublayout of ((value) => value) (...??)
|}]


module type M = sig
  type r : (value => value) => value
  type s : value => value
  val g : list s r -> list s r
end

[%%expect{|
Line 4, characters 10-14:
4 |   val g : list s r -> list s r
              ^^^^
Error: This type list should be an instance of type ('a : value)
       The layout of list is ((value) => value) (...??)
       But the layout of list must be a sublayout of value, because
         of the definition of s at line 3, characters 2-25.
|}]
