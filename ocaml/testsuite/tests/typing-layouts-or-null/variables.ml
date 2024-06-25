(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type t_value_or_null : value_or_null
type ('a : value_or_null) id_value_or_null = 'a


[%%expect{|
type t_value_or_null : value_or_null
type ('a : value_or_null) id_value_or_null = 'a
|}]

(* CR layouts v3.0: sort variables in functions should start as [Or_null]. *)

type 'a should_accept_or_null = 'a id_value_or_null

type should_work = t_value_or_null should_accept_or_null

[%%expect{|
type 'a should_accept_or_null = 'a id_value_or_null
Line 3, characters 19-34:
3 | type should_work = t_value_or_null should_accept_or_null
                       ^^^^^^^^^^^^^^^
Error: This type t_value_or_null should be an instance of type ('a : value)
       The layout of t_value_or_null is value_or_null, because
         of the definition of t_value_or_null at line 1, characters 0-36.
       But the layout of t_value_or_null must be a sublayout of value, because
         of the definition of should_accept_or_null at line 1, characters 0-51.
|}]

let should_work (x : t_value_or_null) = x

[%%expect{|
Line 1, characters 16-37:
1 | let should_work (x : t_value_or_null) = x
                    ^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type t_value_or_null
       but a pattern was expected which matches values of type ('a : value)
       The layout of t_value_or_null is value_or_null, because
         of the definition of t_value_or_null at line 1, characters 0-36.
       But the layout of t_value_or_null must be a sublayout of value, because
         we must know concretely how to pass a function argument, defaulted to layout value.
|}]

module type S = sig
  val should_work : 'a -> unit
end

module M (X : S) : sig
  val should_work : ('a : value_or_null) . 'a -> unit
end = X

[%%expect{|
module type S = sig val should_work : 'a -> unit end
Line 7, characters 6-7:
7 | end = X
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val should_work : 'a -> unit end
       is not included in
         sig val should_work : ('a : value_or_null). 'a -> unit end
       Values do not match:
         val should_work : 'a -> unit
       is not included in
         val should_work : ('a : value_or_null). 'a -> unit
       The type 'a -> unit is not compatible with the type 'b -> unit
       The layout of 'a is value_or_null, because
         of the definition of should_work at line 6, characters 2-53.
       But the layout of 'a must be a sublayout of value, because
         of the definition of should_work at line 2, characters 2-30.
|}]

(* Type parameters should default to [value] for fully abstract types *)

module M (X : sig type 'a t end) : sig type ('a : value_or_null) t end = X

[%%expect{|
Line 1, characters 73-74:
1 | module M (X : sig type 'a t end) : sig type ('a : value_or_null) t end = X
                                                                             ^
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a X.t end
       is not included in
         sig type ('a : value_or_null) t end
       Type declarations do not match:
         type 'a t = 'a X.t
       is not included in
         type ('a : value_or_null) t
       Their parameters differ:
       The type ('a : value) is not equal to the type ('a0 : value_or_null)
       because their layouts are different.
|}]

(* CR layouts v3.0: type parameters should default to [any] for abstract types
   with equalities. *)

module type S = sig
  type 'a t = 'a

  type t2 = t_value_or_null t
end

[%%expect{|
Line 4, characters 12-27:
4 |   type t2 = t_value_or_null t
                ^^^^^^^^^^^^^^^
Error: This type t_value_or_null should be an instance of type ('a : value)
       The layout of t_value_or_null is value_or_null, because
         of the definition of t_value_or_null at line 1, characters 0-36.
       But the layout of t_value_or_null must be a sublayout of value, because
         of the definition of t at line 2, characters 2-16.
|}]

(* CR layouts v3.0: the sublayout check should accept this for backwards
   compatibility. *)

module M : sig
  type 'a t
end = struct
  type ('a : value_or_null) t = 'a
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : value_or_null) t = 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a : value_or_null) t = 'a end
       is not included in
         sig type 'a t end
       Type declarations do not match:
         type ('a : value_or_null) t = 'a
       is not included in
         type 'a t
       Their parameters differ:
       The type ('a : value_or_null) is not equal to the type ('a0 : value)
       because their layouts are different.
|}]

(* CR layouts v3.0: type parameters should default to [any] for
   non-abstract types. *)

module type S = sig
  type 'a t = Value of 'a

  type t2 = t_value_or_null t
end

[%%expect{|
Line 4, characters 12-27:
4 |   type t2 = t_value_or_null t
                ^^^^^^^^^^^^^^^
Error: This type t_value_or_null should be an instance of type ('a : value)
       The layout of t_value_or_null is value_or_null, because
         of the definition of t_value_or_null at line 1, characters 0-36.
       But the layout of t_value_or_null must be a sublayout of value, because
         of the definition of t at line 2, characters 2-25.
|}]
