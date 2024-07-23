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
Line 1, characters 9-12:
1 | type p = t t
             ^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of t is ((value) => value) (...??)
         But the layout of t must be a sublayout of value, because
           of the definition of t at line 1, characters 0-23.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
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
module type M =
  sig
    type r : (value => value) => value
    type s : value => value
    val g : int s r -> int s r
  end
|}]

type ('a : value => value) t = int 'a

module M : sig
  type a = int list
end = struct
  type a = list t
end

[%%expect{|
type ('a : (value) => value) t = <Tapp>
module M : sig type a = int list end
|}]
