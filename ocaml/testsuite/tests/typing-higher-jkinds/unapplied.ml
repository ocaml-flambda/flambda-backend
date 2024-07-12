(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type t : value => value = list

[%%expect {|
type t = list
|}]

type it : value = int t
type it' = int t

[%%expect {|
type it = int t
type it' = int t
|}]

module M : sig
  type k
end = struct
  type k = int t
end

[%%expect {|
module M : sig type k end
|}]

(* TODO jbachurski: Perhaps this should work. *)
type t = list

[%%expect {|
Line 1, characters 0-13:
1 | type t = list
    ^^^^^^^^^^^^^
Error: The layout of type list is ((value) => value) (...??)
       But the layout of type list must be a sublayout of any, because
         of the definition of t at line 1, characters 0-13.
|}]

type q = list list

[%%expect {|
Line 1, characters 9-13:
1 | type q = list list
             ^^^^
Error: This type list should be an instance of type ('a : value)
       The layout of list is ((value) => value) (...??)
       But the layout of list must be a sublayout of value, because
         the type argument of list has layout value.
|}]

module M : sig
  type t : value => value
end = struct
  type t : value => value = list
end

[%%expect {|
module M : sig type t : (value) => value end
|}]

module M : sig
  type t : (value, value) => value
end = struct
  type t : value => value = list
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : value => value = list
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = list end
       is not included in
         sig type t : (value, value) => value end
       Type declarations do not match:
         type t = list
       is not included in
         type t : (value, value) => value
       The layout of the first is ((value) => value) (...??)
       But the layout of the first must be a sublayout of ((value, value) => value) (...??)
|}]

(* FIXME jbachurski: This should fail at list, not t. *)
type s = t list

[%%expect {|
Line 1, characters 9-10:
1 | type s = t list
             ^
Error: This type t = list should be an instance of type ('a : value)
       The layout of t is ((value) => value) (...??)
       But the layout of t must be a sublayout of value, because
         the type argument of list has layout value.
|}]

module type M = sig
  val f : ('a : value => value). 'a -> 'a
end

[%%expect {|
module type M = sig val f : ('a : ((higher))). 'a -> 'a end
|}]

type r : (value => value) => value

[%%expect {|
type r : ((value) => value) => value
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
