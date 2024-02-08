(* TEST
   * expect
   flags = "-extension layouts"
   * expect
   flags = "-extension layouts_beta"
*)

module F = Stdlib__Float_u

type t_any : any
type ('a : any) t_with_any = 'a
module M_any : sig
  type ('a : any) t = private 'a
end = struct
  type ('a : any) t = 'a
end
[%%expect{|
module F = Stdlib__Float_u
type t_any : any
type ('a : any) t_with_any = 'a
module M_any : sig type ('a : any) t = private 'a end
|}]

external[@rep_poly] id : ('a : any). 'a -> 'a = "%identity"

let f () = id (assert false : t_any)
[%%expect{|
external id : ('a : any). 'a -> 'a = "%identity" [@@rep_poly]
Line 3, characters 14-36:
3 | let f () = id (assert false : t_any)
                  ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type t_any but an expression was expected of type
         ('a : '_representable_layout_1)
       The layout of t_any is any, because
         of the definition of t_any at line 3, characters 0-16.
       But the layout of t_any must be representable, because
         it's the representation polymorphic type in an external declaration.
|}]

type ('a : any) t
external[@rep_poly] id : ('a : any). 'a t -> 'a t = "%identity"
let f () = id (assert false : t_any t)
[%%expect{|
type ('a : any) t
external id : ('a : any). 'a t -> 'a t = "%identity" [@@rep_poly]
Line 3, characters 14-38:
3 | let f () = id (assert false : t_any t)
                  ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type t_any t
       but an expression was expected of type 'a t
       The layout of t_any is any, because
         of the definition of t_any at line 3, characters 0-16.
       But the layout of t_any must be representable, because
         it's the representation polymorphic type in an external declaration.
|}]


external[@rep_poly] id : ('a : any). 'a -> 'a = "%identity"
(* This works *)
let () = Format.printf "%f %s\n" (F.to_float (id #1.)) (id "abc"); Format.print_flush ()
[%%expect{|
external id : ('a : any). 'a -> 'a = "%identity" [@@rep_poly]
1.000000 abc
|}]

module M = struct
  let id' x = id x
  (* But not this *)
  let () = Format.printf "%f %s\n" (F.to_float (id' #1.)) (id' "abc")
end
[%%expect{|
Line 4, characters 63-68:
4 |   let () = Format.printf "%f %s\n" (F.to_float (id' #1.)) (id' "abc")
                                                                   ^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : float64)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of float64, because
         of the definition of id' at line 2, characters 10-18.
|}]

(********************)
(* Module inclusion *)

(* External in both *)
module S : sig
  external[@rep_poly] id : ('a : any). 'a -> 'a = "%identity"
end = struct
  external[@rep_poly] id : ('a : any). 'a -> 'a = "%identity"
end

[%%expect{|
module S :
  sig external id : ('a : any). 'a -> 'a = "%identity" [@@rep_poly] end
|}]

(* together with local_opt *)
module S : sig
  external[@rep_poly] id : ('a : any). ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
end = struct
  external[@rep_poly] id : ('a : any). ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
end

[%%expect{|
module S :
  sig
    external id : ('a : any). ('a [@local_opt]) -> ('a [@local_opt])
      = "%identity" [@@rep_poly]
  end
|}]


module S : sig
  external id : ('a : any). 'a -> 'a = "%identity"
end = struct
  external[@rep_poly] id : ('a : any). 'a -> 'a = "%identity"
end

[%%expect{|
Line 2, characters 28-30:
2 |   external id : ('a : any). 'a -> 'a = "%identity"
                                ^^
Error: Types in an external must have a representable layout.
       The layout of 'a is any, because
         of the annotation on the universal variable 'a.
       But the layout of 'a must be representable, because
         it's the type of an argument in an external declaration.
|}]

module S : sig
  external[@rep_poly] id : ('a : any). 'a -> 'a = "%identity"
end = struct
  external id : ('a : any). 'a -> 'a = "%identity"
end

[%%expect{|
Line 4, characters 28-30:
4 |   external id : ('a : any). 'a -> 'a = "%identity"
                                ^^
Error: Types in an external must have a representable layout.
       The layout of 'a is any, because
         of the annotation on the universal variable 'a.
       But the layout of 'a must be representable, because
         it's the type of an argument in an external declaration.
|}]

(* External in struct *)

module S : sig
  val id : ('a : float64). 'a -> 'a
end = struct
  external[@rep_poly] id : ('a : any). 'a -> 'a = "%identity"
end

let () = Format.printf "%f\n" (F.to_float (S.id #1.)); Format.print_flush ()

[%%expect{|
module S : sig val id : ('a : float64). 'a -> 'a end
1.000000
|}]

let () = Format.printf "%s\n" (S.id "abc")

[%%expect{|
Line 1, characters 36-41:
1 | let () = Format.printf "%s\n" (S.id "abc")
                                        ^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : float64)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of float64, because
         of the definition of id at line 2, characters 2-35.
|}]


module S : sig
  val id : ('a : any). 'a -> 'a
end = struct
  external[@rep_poly] id : ('a : any). 'a -> 'a = "%identity"
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external[@rep_poly] id : ('a : any). 'a -> 'a = "%identity"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           external id : ('a : any). 'a -> 'a = "%identity" [@@rep_poly]
         end
       is not included in
         sig val id : ('a : any). 'a -> 'a end
       Values do not match:
         external id : ('a : any). 'a -> 'a = "%identity" [@@rep_poly]
       is not included in
         val id : ('a : any). 'a -> 'a
       The type 'a -> 'a is not compatible with the type 'b -> 'b
       The layout of 'a is any, because
         of the definition of id at line 2, characters 2-31.
       But the layout of 'a must be representable, because
         it's the representation polymorphic type in an external declaration.
|}]


(* External in sig *)
module S : sig
  external[@rep_poly] id : ('a : any). 'a -> 'a = "%identity"
end = struct
  let id: ('a : any). 'a -> 'a = assert false
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let id: ('a : any). 'a -> 'a = assert false
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val id : ('a : any). 'a -> 'a end
       is not included in
         sig
           external id : ('a : any). 'a -> 'a = "%identity" [@@rep_poly]
         end
       Values do not match:
         val id : ('a : any). 'a -> 'a
       is not included in
         external id : ('a : any). 'a -> 'a = "%identity" [@@rep_poly]
       The implementation is not a primitive.
|}]

(********************)
(* Variable capture *)

let f (type a : any) () =
  let module M = struct
    external[@rep_poly] id : ('a : any). 'a -> a -> 'a = "%apply"
  end in
  M

[%%expect{|
Line 3, characters 47-48:
3 |     external[@rep_poly] id : ('a : any). 'a -> a -> 'a = "%apply"
                                                   ^
Error: Types in an external must have a representable layout.
       The layout of a is any, because
         of the annotation on the abstract type declaration for a.
       But the layout of a must be representable, because
         it's the type of an argument in an external declaration.
|}]

let f (type a : any) () =
  let module M = struct
    external[@rep_poly] id : ('a : any). 'a -> a t_with_any -> 'a = "%apply"
  end in
  M

[%%expect{|
Line 3, characters 47-59:
3 |     external[@rep_poly] id : ('a : any). 'a -> a t_with_any -> 'a = "%apply"
                                                   ^^^^^^^^^^^^
Error: Types in an external must have a representable layout.
       The layout of a/2 t_with_any is any, because
         of the annotation on the abstract type declaration for a.
       But the layout of a/2 t_with_any must be representable, because
         it's the type of an argument in an external declaration.
|}]

let f (type a : any) () =
  let module M = struct
    external[@rep_poly] id : ('a : any). 'a -> a M_any.t -> 'a = "%apply"
  end in
  M

[%%expect{|
Line 3, characters 47-56:
3 |     external[@rep_poly] id : ('a : any). 'a -> a M_any.t -> 'a = "%apply"
                                                   ^^^^^^^^^
Error: Types in an external must have a representable layout.
       The layout of a/3 M_any.t is any, because
         of the annotation on the abstract type declaration for a.
       But the layout of a/3 M_any.t must be representable, because
         it's the type of an argument in an external declaration.
|}]

(************)
(* Functors *)

module M (A : sig
  type t: any
end) = struct
  external[@rep_poly] id : ('a : any). 'a -> A.t -> 'a = "%apply"
end

[%%expect{|
Line 4, characters 45-48:
4 |   external[@rep_poly] id : ('a : any). 'a -> A.t -> 'a = "%apply"
                                                 ^^^
Error: Types in an external must have a representable layout.
       The layout of A.t is any, because
         of the definition of t at line 2, characters 2-13.
       But the layout of A.t must be representable, because
         it's the type of an argument in an external declaration.
|}]

module M (A : sig
  type ('a: any) t = private 'a
end) = struct
  external[@rep_poly] id : ('a : any). 'a A.t -> 'a = "%identity"
  let f1 (): float# = id (assert false : float# A.t)
  let f2 (): int64# = id (assert false : int64# A.t)
  let f3 (): int32# = id (assert false : int32# A.t)
end

[%%expect{|
module M :
  functor (A : sig type ('a : any) t = private 'a end) ->
    sig
      external id : ('a : any). 'a A.t -> 'a = "%identity" [@@rep_poly]
      val f1 : unit -> float#
      val f2 : unit -> int64#
      val f3 : unit -> int32#
    end
|}]

(*************************)
(* Not allowed in C stub *)

(* Also means [No_native_primitive_with_non_value] errors
   get shadowed when using the attribute. *)

external[@rep_poly] id : ('a : any). 'a -> 'a = "caml_obj_tag"
[%%expect{|
Line 1, characters 0-62:
1 | external[@rep_poly] id : ('a : any). 'a -> 'a = "caml_obj_tag"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Attribute [@rep_poly] can only be used on built-in primitives.
|}]

external[@rep_poly] id : ('a : any). 'a -> 'a = "caml_obj_tag" "caml_obj_tag"
[%%expect{|
Line 1, characters 0-77:
1 | external[@rep_poly] id : ('a : any). 'a -> 'a = "caml_obj_tag" "caml_obj_tag"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Attribute [@rep_poly] can only be used on built-in primitives.
|}]

(***********************************)
(* All type vars get the same sort *)

external[@rep_poly] id : ('a : any) ('b : any). 'a -> 'b = "%identity"
let f (x: float#): int64# = id x

[%%expect{|
external id : ('a : any) ('b : any). 'a -> 'b = "%identity" [@@rep_poly]
Line 2, characters 28-32:
2 | let f (x: float#): int64# = id x
                                ^^^^
Error: This expression has type ('a : float64)
       but an expression was expected of type int64#
       The layout of int64# is bits64, because
         it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of float64, because
         it's the representation polymorphic type in an external declaration, defaulted to layout float64.
|}]
(* CR layouts v2.9: the default part is not quite correct *)

(*************************************)
(* Interaction with other attributes *)

external[@rep_poly] id : ('a : any). 'a -> 'a = "%identity" [@@unboxed]

[%%expect{|
Line 1, characters 37-39:
1 | external[@rep_poly] id : ('a : any). 'a -> 'a = "%identity" [@@unboxed]
                                         ^^
Error: Don't know how to unbox this type.
       Only float, int32, int64, nativeint, and vector primitives can be unboxed.
|}]

external[@rep_poly] id : ('a : any). 'a -> 'a = "%identity" [@@untagged]

[%%expect{|
Line 1, characters 37-39:
1 | external[@rep_poly] id : ('a : any). 'a -> 'a = "%identity" [@@untagged]
                                         ^^
Error: Don't know how to untag this type. Only int can be untagged.
|}]

external[@rep_poly] id : ('a : any). 'a -> 'a =
  "%identity" "%identity" "float"
[%%expect{|
Lines 1-2, characters 0-33:
1 | external[@rep_poly] id : ('a : any). 'a -> 'a =
2 |   "%identity" "%identity" "float"
Error: Cannot use "float" in conjunction with types of non-value layouts.
|}]

(*************************************)
(* Type var in nested in other types *)

external[@rep_poly] id : ('a : any). 'a t_with_any -> 'a t_with_any = "%identity"

let f (x: float#): float# = id x
let f (x: int64#): int64# = id x
let f (x: int32#): int32# = id x

[%%expect{|
external id : ('a : any). 'a t_with_any -> 'a t_with_any = "%identity"
  [@@rep_poly]
val f : float# -> float# = <fun>
val f : int64# -> int64# = <fun>
val f : int32# -> int32# = <fun>
|}]


external[@rep_poly] id : ('a : any). 'a M_any.t -> 'a M_any.t = "%identity"

let f (): float# M_any.t = id (assert false : float# M_any.t)
let f (): int64# M_any.t = id (assert false : int64# M_any.t)
let f (): int32# M_any.t = id (assert false : int32# M_any.t)

[%%expect{|
external id : ('a : any). 'a M_any.t -> 'a M_any.t = "%identity" [@@rep_poly]
val f : unit -> float# M_any.t = <fun>
val f : unit -> int64# M_any.t = <fun>
val f : unit -> int32# M_any.t = <fun>
|}]


(* doesn't work when the type constructor puts a constraint on ['a] *)
external[@rep_poly] id : ('a : any). 'a list -> 'a list = "%identity"

[%%expect{|
Line 1, characters 25-55:
1 | external[@rep_poly] id : ('a : any). 'a list -> 'a list = "%identity"
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have layout any.
       But it was inferred to have layout value, because
         the type argument of list has layout value.
|}]

(* Test this when sorts can be inside unboxed records *)
(* type ('a : any) r = {field: 'a} [@@unboxed]
external[@rep_poly] id : ('a : any). 'a M_any.t r -> 'a M_any.t r = "%identity"

let f (): float# M_any.t r = id (assert false : float# M_any.t r)
let f (): int64# M_any.t r = id (assert false : int64# M_any.t r)
let f (): int32# M_any.t r = id (assert false : int32# M_any.t r) *)


(********************************************)
(* Some primitives require rep_poly to work *)

type ('a : any) t
external id : ('a : any). 'a t -> int = "%array_length"
let id' x = id x

[%%expect{|
type ('a : any) t
Line 2, characters 14-37:
2 | external id : ('a : any). 'a t -> int = "%array_length"
                  ^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%array_length] doesn't work well with type variables of
       layout any. Consider using [@rep_poly].
|}]

external[@rep_poly] id : ('a : any). 'a t -> int = "%array_length"
let id' x = id x

[%%expect{|
external id : ('a : any). 'a t -> int = "%array_length" [@@rep_poly]
val id' : 'a t -> int = <fun>
|}]

external id : ('a : any). 'a t -> int = "%identity"
let id' x = id x

[%%expect{|
external id : ('a : any). 'a t -> int = "%identity"
val id' : ('a : any). 'a t -> int = <fun>
|}]
