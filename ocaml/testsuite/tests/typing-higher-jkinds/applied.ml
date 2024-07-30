(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type t : value => value

[%%expect {|
type t : value => value
|}]


type p = int t

[%%expect {|
type p = int t
|}]


module M : sig
  type p = int t
end = struct
  type p = int t
end

[%%expect {|
module M : sig type p = int t end
|}]


module M : sig
  type 'a p = 'a t
end = struct
  type 'a p = 'a t
end

[%%expect {|
module M : sig type 'a p = 'a t end
|}]


module M : sig
  type p
end = struct
  type p = int t
end

[%%expect {|
module M : sig type p end
|}]


type p = t t

[%%expect {|
Line 1, characters 9-10:
1 | type p = t t
             ^
Error: This type t should be an instance of type ('a : value)
       The layout of t is ((value) => value), because
         of the definition of t at line 1, characters 0-23.
       But the layout of t must be a sublayout of value, because
         of the definition of t at line 1, characters 0-23.
|}]


type r : (value => value) => value

[%%expect {|
type r : (value => value) => value
|}]


type ('a : value => value) r

[%%expect {|
type ('a : value => value) r
|}]


module type M = sig
  val g : ('a : value => value). 'a r -> 'a r
end

[%%expect{|
module type M = sig val g : ('a : value => value). 'a r -> 'a r end
|}]


module type M = sig
  type r : (value => value) => value
  type s : value => value
  val g : s r -> s r
end

[%%expect{|
module type M =
  sig
    type r : (value => value) => value
    type s : value => value
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
Error: This type int s should be an instance of type ('a : value => value)
       The layout of int s is value, because
         of the definition of s at line 3, characters 2-25.
       But the layout of int s must be a sublayout of ((value) => value), because
         of the definition of r at line 2, characters 2-36.
|}]

type t : value => value
let f (x : 'a t) = x
[%%expect {|
type t : value => value
val f : 'a t -> 'a t = <fun>
|}]

(* Not a datatype *)
type 'a t
let f (x : 'a t) = x
[%%expect {|
type 'a t
val f : 'a t -> 'a t = <fun>
|}]

type ('a : value => value) t = int 'a

module M : sig
  type a = int list
end = struct
  type a = list t
end

[%%expect{|
type ('a : value => value) t = int 'a
module M : sig type a = int list end
|}]
