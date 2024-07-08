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

(* Type parameters default to [value] and need
   explicit annotations to accept [value_or_null]. *)

type 'a should_not_accept_or_null = 'a id_value_or_null

type should_not_work = t_value_or_null should_not_accept_or_null

[%%expect{|
type 'a should_not_accept_or_null = 'a id_value_or_null
Line 3, characters 23-38:
3 | type should_not_work = t_value_or_null should_not_accept_or_null
                           ^^^^^^^^^^^^^^^
Error: This type t_value_or_null should be an instance of type ('a : value)
       The kind of t_value_or_null is value_or_null, because
         of the definition of t_value_or_null at line 1, characters 0-36.
       But the kind of t_value_or_null must be a subkind of value, because
         of the definition of should_not_accept_or_null at line 1, characters 0-55.
|}]

(* CR layouts v3.0: [value_or_null] types should be accepted for
   function arguments and results. *)

let should_work (x : t_value_or_null) = x

[%%expect{|
Line 1, characters 16-37:
1 | let should_work (x : t_value_or_null) = x
                    ^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type t_value_or_null
       but a pattern was expected which matches values of type ('a : value)
       The kind of t_value_or_null is value_or_null, because
         of the definition of t_value_or_null at line 1, characters 0-36.
       But the kind of t_value_or_null must be a subkind of value, because
         we must know concretely how to pass a function argument,
         defaulted to kind value.
|}]

(* Type variables in function definitions default to [value]. *)

module type S = sig
  val should_not_work : 'a -> unit
end

module M (X : S) : sig
  val should_not_work : ('a : value_or_null) . 'a -> unit
end = X

[%%expect{|
module type S = sig val should_not_work : 'a -> unit end
Line 7, characters 6-7:
7 | end = X
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val should_not_work : 'a -> unit end
       is not included in
         sig val should_not_work : ('a : value_or_null). 'a -> unit end
       Values do not match:
         val should_not_work : 'a -> unit
       is not included in
         val should_not_work : ('a : value_or_null). 'a -> unit
       The type 'a -> unit is not compatible with the type 'b -> unit
       The kind of 'a is value_or_null, because
         of the definition of should_not_work at line 6, characters 2-57.
       But the kind of 'a must be a subkind of value, because
         of the definition of should_not_work at line 2, characters 2-34.
|}]

(* Type parameters default to [value] for fully abstract types *)

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

(* Ttype parameters default to [value] for abstract types with equalities. *)

module type S = sig
  type 'a t = 'a

  type t2 = t_value_or_null t
end

[%%expect{|
Line 4, characters 12-27:
4 |   type t2 = t_value_or_null t
                ^^^^^^^^^^^^^^^
Error: This type t_value_or_null should be an instance of type ('a : value)
       The kind of t_value_or_null is value_or_null, because
         of the definition of t_value_or_null at line 1, characters 0-36.
       But the kind of t_value_or_null must be a subkind of value, because
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

(* Type parameters default to [value] for non-abstract types. *)

module type S = sig
  type 'a t = Value of 'a

  type t2 = t_value_or_null t
end

[%%expect{|
Line 4, characters 12-27:
4 |   type t2 = t_value_or_null t
                ^^^^^^^^^^^^^^^
Error: This type t_value_or_null should be an instance of type ('a : value)
       The kind of t_value_or_null is value_or_null, because
         of the definition of t_value_or_null at line 1, characters 0-36.
       But the kind of t_value_or_null must be a subkind of value, because
         of the definition of t at line 2, characters 2-25.
|}]

(* Rigid type variables default to [value]. *)

module M : sig
  val f : ('a : value_or_null). 'a -> 'a
end = struct
  let f (type a) (x : a) = x
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (type a) (x : a) = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end
       is not included in
         sig val f : ('a : value_or_null). 'a -> 'a end
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : ('a : value_or_null). 'a -> 'a
       The type 'a -> 'a is not compatible with the type 'b -> 'b
       The kind of 'a is value_or_null, because
         of the definition of f at line 2, characters 2-40.
       But the kind of 'a must be a subkind of value, because
         of the definition of f at line 4, characters 8-28.
|}]

module M : sig
  val f : ('a : value_or_null). 'a -> 'a
end = struct
  let f : 'a. 'a -> 'a = fun x -> x
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f : 'a. 'a -> 'a = fun x -> x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end
       is not included in
         sig val f : ('a : value_or_null). 'a -> 'a end
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : ('a : value_or_null). 'a -> 'a
       The type 'a -> 'a is not compatible with the type 'b -> 'b
       The kind of 'a is value_or_null, because
         of the definition of f at line 2, characters 2-40.
       But the kind of 'a must be a subkind of value, because
         of the definition of f at line 4, characters 6-7.
|}]


module M : sig
  val f : ('a : value_or_null) . 'a -> 'a
end = struct
  let f : type a. a -> a = fun x -> x
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f : type a. a -> a = fun x -> x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end
       is not included in
         sig val f : ('a : value_or_null). 'a -> 'a end
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : ('a : value_or_null). 'a -> 'a
       The type 'a -> 'a is not compatible with the type 'b -> 'b
       The kind of 'a is value_or_null, because
         of the definition of f at line 2, characters 2-41.
       But the kind of 'a must be a subkind of value, because
         of the definition of f at line 4, characters 6-7.
|}]

(* CR layouts v3.0: this should work. *)

module M : sig
  val f : ('a : value_or_null). 'a -> 'a
end = struct
  let f x = x
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end
       is not included in
         sig val f : ('a : value_or_null). 'a -> 'a end
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : ('a : value_or_null). 'a -> 'a
       The type 'a -> 'a is not compatible with the type 'b -> 'b
       The kind of 'a is value_or_null, because
         of the definition of f at line 2, characters 2-40.
       But the kind of 'a must be a subkind of value, because
         of the definition of f at line 4, characters 8-13.
|}]

(* CR layouts v3.0: annotations on non-rigid type variables are upper bounds.
   This is in line with similar OCaml behavior, but is confusing. *)

module M : sig
  val f : ('a : value_or_null) -> 'a
end = struct
  let f (type a) (x : a) = x
end

[%%expect{|
module M : sig val f : 'a -> 'a end
|}]

(* GADTs and constraints tests. *)

type (!'a : value_or_null) dummy

(* This must infer ('a : value) for backwards compatibility. *)
type t = Packed : 'a dummy -> t

[%%expect{|
type (!'a : value_or_null) dummy
type t = Packed : 'a dummy -> t
|}]

(* Annotations here are upper bounds, so due to defaulting we can't
   set ['a : value_or_null]. *)
type t = Packed : ('a : value_or_null) dummy -> t
[%%expect{|
type t = Packed : 'a dummy -> t
|}]

(* However, this works. *)
type t = Packed : ('a : value_or_null). 'a dummy -> t
[%%expect{|
type t = Packed : ('a : value_or_null). 'a dummy -> t
|}]

(* Variables on the right side of constraints default to non-null.
   This must be the case for backwards compatibility: ['b constrained]
   below should be a [value] to allow upgrading existing types like [list]
   to accept [value_or_null]. *)
type 'b constrained = 'a constraint 'b = 'a dummy

[%%expect{|
type 'b constrained = 'a constraint 'b = 'a dummy
|}]

type fails = bool constrained

[%%expect{|
Line 1, characters 13-17:
1 | type fails = bool constrained
                 ^^^^
Error: This type bool should be an instance of type 'a dummy
|}]

type fails = (t_value_or_null dummy) constrained

[%%expect{|
Line 1, characters 14-35:
1 | type fails = (t_value_or_null dummy) constrained
                  ^^^^^^^^^^^^^^^^^^^^^
Error: This type t_value_or_null dummy should be an instance of type 'a dummy
       The kind of t_value_or_null is value_or_null, because
         of the definition of t_value_or_null at line 1, characters 0-36.
       But the kind of t_value_or_null must be a subkind of value, because
         of the definition of constrained at line 1, characters 0-49.
|}]

type succeeds = (int dummy) constrained

[%%expect{|
type succeeds = int dummy constrained
|}]

(* CR layouts v3.0: we can't set a variable on the right side of
   the constraint to be [maybe_null]. This might be hard to fix, see
   [Note about [new_var_jkind]]. *)
type ('c : value_or_null) constrained' = bool
  constraint 'c = ('a : value_or_null) dummy

[%%expect{|
type 'b constrained' = bool constraint 'b = 'a dummy
|}]

type fails = (t_value_or_null dummy) constrained'

[%%expect{|
Line 1, characters 14-35:
1 | type fails = (t_value_or_null dummy) constrained'
                  ^^^^^^^^^^^^^^^^^^^^^
Error: This type t_value_or_null dummy should be an instance of type 'a dummy
       The kind of t_value_or_null is value_or_null, because
         of the definition of t_value_or_null at line 1, characters 0-36.
       But the kind of t_value_or_null must be a subkind of value, because
         of the definition of constrained' at lines 1-2, characters 0-44.
|}]
