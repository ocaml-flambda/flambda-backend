(* TEST
 include stdlib_upstream_compatible;
 {
   flags = "-extension layouts";
   expect;
 }{
   flags = "-extension layouts_beta";
   expect;
 }
*)

module F = Stdlib_upstream_compatible.Float_u

type t_any : any
type ('a : any) t_with_any = 'a
module M_any : sig
  type ('a : any) t = private 'a
end = struct
  type ('a : any) t = 'a
end
[%%expect{|
module F = Stdlib_upstream_compatible.Float_u
type t_any : any
type ('a : any) t_with_any = 'a
module M_any : sig type ('a : any) t = private 'a end
|}]

external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"

let f () = id (assert false : t_any)
[%%expect{|
external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
Line 3, characters 14-36:
3 | let f () = id (assert false : t_any)
                  ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type t_any but an expression was expected of type
         ('a : '_representable_layout_1)
       The layout of t_any is any
         because of the definition of t_any at line 3, characters 0-16.
       But the layout of t_any must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

type ('a : any) t
external[@layout_poly] id : ('a : any). 'a t -> 'a t = "%identity"
let f () = id (assert false : t_any t)
[%%expect{|
type ('a : any) t
external id : ('a : any). 'a t -> 'a t = "%identity" [@@layout_poly]
Line 3, characters 14-38:
3 | let f () = id (assert false : t_any t)
                  ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type t_any t
       but an expression was expected of type 'a t
       The layout of t_any is any
         because of the definition of t_any at line 3, characters 0-16.
       But the layout of t_any must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]


external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"
(* This works *)
let () = Format.printf "%f %s\n" (F.to_float (id #1.)) (id "abc"); Format.print_flush ()
[%%expect{|
external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
1.000000 abc
|}]

module M = struct
  let id' x = id x
  (* But not this *)
  let () = Format.printf "%f %s\n" (F.to_float (id' #1.)) (id' "abc")
end

(* CR layouts v2.8: The jkind in the error message is wrong. It should really be
   ('a : layout float64) *)
[%%expect{|
Line 4, characters 63-68:
4 |   let () = Format.printf "%f %s\n" (F.to_float (id' #1.)) (id' "abc")
                                                                   ^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : float64)
       The layout of string is value
         because it is the primitive immutable_data type string.
       But the layout of string must be a sublayout of float64
         because of the definition of id' at line 2, characters 10-18.
|}]

(********************)
(* Module inclusion *)

(* External in both *)
module S : sig
  external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"
end = struct
  external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"
end

let g1 () = S.id #1.0
let g2 () = S.id "abc"

[%%expect{|
module S :
  sig external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly] end
val g1 : unit -> float# = <fun>
val g2 : unit -> string = <fun>
|}]

type ('a : any) s
module S : sig
  external id : ('a : any). 'a s -> 'a s = "%identity"
end = struct
  external[@layout_poly] id : ('a : any). 'a s -> 'a s = "%identity"
end

[%%expect{|
type ('a : any) s
Lines 4-6, characters 6-3:
4 | ......struct
5 |   external[@layout_poly] id : ('a : any). 'a s -> 'a s = "%identity"
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           external id : ('a : any). 'a s -> 'a s = "%identity"
             [@@layout_poly]
         end
       is not included in
         sig external id : ('a : any). 'a s -> 'a s = "%identity" end
       Values do not match:
         external id : ('a : any). 'a s -> 'a s = "%identity" [@@layout_poly]
       is not included in
         external id : ('a : any). 'a s -> 'a s = "%identity"
       The type 'a s -> 'a s is not compatible with the type 'b s -> 'b s
       The layout of 'a is any
         because of the definition of id at line 3, characters 2-54.
       But the layout of 'a must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

type ('a : any, 'b : any) s
type t_any : any
module S : sig
  external[@layout_poly] id :
    ('b : any). (t_any, 'b) s -> (t_any, 'b) s = "%identity"
end = struct
  external[@layout_poly] id :
    ('a : any) ('b : any). ('a, 'b) s -> ('a, 'b) s = "%identity"
end

[%%expect{|
type ('a : any, 'b : any) s
type t_any : any
Lines 6-9, characters 6-3:
6 | ......struct
7 |   external[@layout_poly] id :
8 |     ('a : any) ('b : any). ('a, 'b) s -> ('a, 'b) s = "%identity"
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           external id : ('a : any) ('b : any). ('a, 'b) s -> ('a, 'b) s
             = "%identity" [@@layout_poly]
         end
       is not included in
         sig
           external id : ('b : any). (t_any, 'b) s -> (t_any, 'b) s
             = "%identity" [@@layout_poly]
         end
       Values do not match:
         external id : ('a : any) ('b : any). ('a, 'b) s -> ('a, 'b) s
           = "%identity" [@@layout_poly]
       is not included in
         external id : ('b : any). (t_any, 'b) s -> (t_any, 'b) s
           = "%identity" [@@layout_poly]
       The type ('a, 'b) s -> ('a, 'b) s is not compatible with the type
         (t_any, 'c) s -> (t_any, 'c) s
       The layout of t_any is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of t_any must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

type ('a : any, 'b : any) s

type t_any : any
module S : sig
  external[@layout_poly] id :
    ('a : any) ('b : any). ('a, 'b) s -> ('a, 'b) s = "%identity"
end = struct
  external[@layout_poly] id :
    ('b : any). (t_any, 'b) s -> (t_any, 'b) s = "%identity"
end

[%%expect{|
type ('a : any, 'b : any) s
type t_any : any
Lines 7-10, characters 6-3:
 7 | ......struct
 8 |   external[@layout_poly] id :
 9 |     ('b : any). (t_any, 'b) s -> (t_any, 'b) s = "%identity"
10 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           external id : ('b : any). (t_any, 'b) s -> (t_any, 'b) s
             = "%identity" [@@layout_poly]
         end
       is not included in
         sig
           external id : ('a : any) ('b : any). ('a, 'b) s -> ('a, 'b) s
             = "%identity" [@@layout_poly]
         end
       Values do not match:
         external id : ('b : any). (t_any, 'b) s -> (t_any, 'b) s
           = "%identity" [@@layout_poly]
       is not included in
         external id : ('a : any) ('b : any). ('a, 'b) s -> ('a, 'b) s
           = "%identity" [@@layout_poly]
       The type (t_any, 'a) s -> (t_any, 'a) s
       is not compatible with the type ('b, 'c) s -> ('b, 'c) s
       Type t_any is not compatible with type 'b
|}]

(* together with local_opt *)
module S : sig
  external[@layout_poly] id : ('a : any). ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
end = struct
  external[@layout_poly] id : ('a : any). ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
end

let g1 () = S.id #1.0
let g2 () = S.id "abc"

[%%expect{|
module S :
  sig
    external id : ('a : any). ('a [@local_opt]) -> ('a [@local_opt])
      = "%identity" [@@layout_poly]
  end
val g1 : unit -> float# = <fun>
val g2 : unit -> string = <fun>
|}]


module S : sig
  external id : ('a : any). 'a -> 'a = "%identity"
end = struct
  external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"
end

[%%expect{|
Line 2, characters 28-30:
2 |   external id : ('a : any). 'a -> 'a = "%identity"
                                ^^
Error: Types in an external must have a representable layout.
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because it's the type of an argument in an external declaration.
|}]

module S : sig
  external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"
end = struct
  external id : ('a : any). 'a -> 'a = "%identity"
end

[%%expect{|
Line 4, characters 28-30:
4 |   external id : ('a : any). 'a -> 'a = "%identity"
                                ^^
Error: Types in an external must have a representable layout.
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because it's the type of an argument in an external declaration.
|}]

(* External in struct *)

module S : sig
  val id : ('a : float64). 'a -> 'a
end = struct
  external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"
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
       The layout of string is value
         because it is the primitive immutable_data type string.
       But the layout of string must be a sublayout of float64
         because of the definition of id at line 2, characters 2-35.
|}]


module S : sig
  val id : ('a : any). 'a -> 'a
end = struct
  external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
         end
       is not included in
         sig val id : ('a : any). 'a -> 'a end
       Values do not match:
         external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
       is not included in
         val id : ('a : any). 'a -> 'a
       The type 'a -> 'a is not compatible with the type 'b -> 'b
       The layout of 'a is any
         because of the definition of id at line 2, characters 2-31.
       But the layout of 'a must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]


(* External in sig *)
module S : sig
  external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"
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
           external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
         end
       Values do not match:
         val id : ('a : any). 'a -> 'a
       is not included in
         external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
       The implementation is not a primitive.
|}]

(********************)
(* Variable capture *)

module type S = sig
  type t
  val f : 'a -> t -> 'a
end

let f (type a1 : any) () =
  let module M = struct
    type t = a1
    external[@layout_poly] f : ('a : any). 'a -> a1 -> 'a = "%apply"
  end in
  (module M : S with type t = a1)

[%%expect{|
module type S = sig type t val f : 'a -> t -> 'a end
Line 9, characters 49-51:
9 |     external[@layout_poly] f : ('a : any). 'a -> a1 -> 'a = "%apply"
                                                     ^^
Error: Types in an external must have a representable layout
       (locally-scoped type variables with layout 'any' are
       made representable by [@layout_poly]).
       The layout of a1 is any
         because of the annotation on the abstract type declaration for a1.
       But the layout of a1 must be representable
         because it's the type of an argument in an external declaration.
|}]

let f (type a2 : any) () =
  let module M = struct
    type t = a2
    external[@layout_poly] f : ('a : any). 'a -> a2 t_with_any -> 'a = "%apply"
  end in
  (module M : S with type t = a2)

[%%expect{|
Line 4, characters 49-62:
4 |     external[@layout_poly] f : ('a : any). 'a -> a2 t_with_any -> 'a = "%apply"
                                                     ^^^^^^^^^^^^^
Error: Types in an external must have a representable layout
       (locally-scoped type variables with layout 'any' are
       made representable by [@layout_poly]).
       The layout of a2 t_with_any is any
         because of the annotation on the abstract type declaration for a2.
       But the layout of a2 t_with_any must be representable
         because it's the type of an argument in an external declaration.
|}]

let f (type a3 : any) () =
  let module M = struct
    type t = a3
    external[@layout_poly] f : ('a : any). 'a -> a3 M_any.t -> 'a = "%apply"
  end in
  (module M : S with type t = a3)

[%%expect{|
Line 4, characters 49-59:
4 |     external[@layout_poly] f : ('a : any). 'a -> a3 M_any.t -> 'a = "%apply"
                                                     ^^^^^^^^^^
Error: Types in an external must have a representable layout
       (locally-scoped type variables with layout 'any' are
       made representable by [@layout_poly]).
       The layout of a3 M_any.t is any
         because of the annotation on the abstract type declaration for a3.
       But the layout of a3 M_any.t must be representable
         because it's the type of an argument in an external declaration.
|}]

module type S4 = sig
  type t
  val f : int -> int
end

let f (type a4 : any) () =
  let module M = struct
    type t = a4
    type ('a : any) s = int
    external[@layout_poly] f : a4 s -> a4 s = "%identity"
  end in
  (module M : S4 with type t = a4)

[%%expect{|
module type S4 = sig type t val f : int -> int end
Line 10, characters 31-43:
10 |     external[@layout_poly] f : a4 s -> a4 s = "%identity"
                                    ^^^^^^^^^^^^
Error: [@layout_poly] on this external declaration has no
       effect. Consider removing it or adding a type
       variable for it to operate on.
|}]

(************)
(* Functors *)

module M (A : sig
  type t: any
end) = struct
  external[@layout_poly] id : ('a : any). 'a -> A.t -> 'a = "%apply"
end

[%%expect{|
Line 4, characters 48-51:
4 |   external[@layout_poly] id : ('a : any). 'a -> A.t -> 'a = "%apply"
                                                    ^^^
Error: Types in an external must have a representable layout
       (locally-scoped type variables with layout 'any' are
       made representable by [@layout_poly]).
       The layout of A.t is any
         because of the definition of t at line 2, characters 2-13.
       But the layout of A.t must be representable
         because it's the type of an argument in an external declaration.
|}]

module M (A : sig
  type ('a: any) t = private 'a
end) = struct
  external[@layout_poly] id : ('a : any). 'a A.t -> 'a = "%identity"
  let f1 (): float# = id (assert false : float# A.t)
  let f2 (): int64# = id (assert false : int64# A.t)
  let f3 (): int32# = id (assert false : int32# A.t)
end

[%%expect{|
module M :
  functor (A : sig type ('a : any) t = private 'a end) ->
    sig
      external id : ('a : any). 'a A.t -> 'a = "%identity" [@@layout_poly]
      val f1 : unit -> float#
      val f2 : unit -> int64#
      val f3 : unit -> int32#
    end
|}]

(*************************)
(* Not allowed in C stub *)

(* Also means [No_native_primitive_with_non_value] errors
   get shadowed when using the attribute. *)

external[@layout_poly] id : ('a : any). 'a -> 'a = "caml_obj_tag"
[%%expect{|
Line 1, characters 0-65:
1 | external[@layout_poly] id : ('a : any). 'a -> 'a = "caml_obj_tag"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Attribute [@layout_poly] can only be used on built-in primitives.
|}]

external[@layout_poly] id : ('a : any). 'a -> 'a = "caml_obj_tag" "caml_obj_tag"
[%%expect{|
Line 1, characters 0-80:
1 | external[@layout_poly] id : ('a : any). 'a -> 'a = "caml_obj_tag" "caml_obj_tag"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Attribute [@layout_poly] can only be used on built-in primitives.
|}]

(***********************************)
(* All type vars get the same sort *)

external[@layout_poly] id : ('a : any) ('b : any). 'a -> 'b = "%identity"
let f (x: float#): int64# = id x

(* CR layouts v2.8: The jkind in the error message is wrong. It should really be
   ('a : layout float64) *)
[%%expect{|
external id : ('a : any) ('b : any). 'a -> 'b = "%identity" [@@layout_poly]
Line 2, characters 28-32:
2 | let f (x: float#): int64# = id x
                                ^^^^
Error: This expression has type ('a : float64)
       but an expression was expected of type int64#
       The layout of int64# is bits64
         because it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of float64
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]
(* CR layouts v2.9: the default part is not quite correct *)

(*************************************)
(* Interaction with other attributes *)

external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity" [@@unboxed]

[%%expect{|
Line 1, characters 40-42:
1 | external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity" [@@unboxed]
                                            ^^
Error: Don't know how to unbox this type.
       Only float, int32, int64, nativeint, vector primitives, and
       concrete unboxed types can be marked unboxed.
|}]

external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity" [@@untagged]

[%%expect{|
Line 1, characters 40-42:
1 | external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity" [@@untagged]
                                            ^^
Error: Don't know how to untag this type. Only int can be untagged.
|}]

external[@layout_poly] id : ('a : any). 'a -> 'a =
  "%identity" "%identity" "float"
[%%expect{|
Lines 1-2, characters 0-33:
1 | external[@layout_poly] id : ('a : any). 'a -> 'a =
2 |   "%identity" "%identity" "float"
Error: Cannot use "float" in conjunction with types of non-value layouts.
|}]

(*************************************)
(* Type var in nested in other types *)

external[@layout_poly] id : ('a : any). 'a t_with_any -> 'a t_with_any = "%identity"

let f (x: float#): float# = id x
let f (x: int64#): int64# = id x
let f (x: int32#): int32# = id x

[%%expect{|
external id : ('a : any). 'a t_with_any -> 'a t_with_any = "%identity"
  [@@layout_poly]
val f : float# -> float# = <fun>
val f : int64# -> int64# = <fun>
val f : int32# -> int32# = <fun>
|}]


external[@layout_poly] id : ('a : any). 'a M_any.t -> 'a M_any.t = "%identity"

let f (): float# M_any.t = id (assert false : float# M_any.t)
let f (): int64# M_any.t = id (assert false : int64# M_any.t)
let f (): int32# M_any.t = id (assert false : int32# M_any.t)

[%%expect{|
external id : ('a : any). 'a M_any.t -> 'a M_any.t = "%identity"
  [@@layout_poly]
val f : unit -> float# M_any.t = <fun>
val f : unit -> int64# M_any.t = <fun>
val f : unit -> int32# M_any.t = <fun>
|}]


(* doesn't work when the type constructor puts a constraint on ['a] *)
external[@layout_poly] id : ('a : any). 'a list -> 'a list = "%identity"

[%%expect{|
Line 1, characters 28-58:
1 | external[@layout_poly] id : ('a : any). 'a list -> 'a list = "%identity"
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind any.
       But it was inferred to have kind value
         because the type argument of list has kind value.
|}]

(* Test this when sorts can be inside unboxed records *)
(* type ('a : any) r = {field: 'a} [@@unboxed]
external[@layout_poly] id : ('a : any). 'a M_any.t r -> 'a M_any.t r = "%identity"

let f (): float# M_any.t r = id (assert false : float# M_any.t r)
let f (): int64# M_any.t r = id (assert false : int64# M_any.t r)
let f (): int32# M_any.t r = id (assert false : int32# M_any.t r) *)


(********************************************)
(* Some primitives require layout_poly to work *)

type ('a : any) t
external id : ('a : any). 'a t -> int = "%array_length"
let id' x = id x

[%%expect{|
type ('a : any) t
Line 2, characters 14-37:
2 | external id : ('a : any). 'a t -> int = "%array_length"
                  ^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%array_length] doesn't work well with type variables of
       layout any. Consider using [@layout_poly].
|}]

external[@layout_poly] id : ('a : any). 'a t -> int = "%array_length"
let id' x = id x

[%%expect{|
external id : ('a : any). 'a t -> int = "%array_length" [@@layout_poly]
val id' : 'a t -> int = <fun>
|}]

external id : ('a : any). 'a t -> int = "%identity"
let id' x = id x

[%%expect{|
external id : ('a : any). 'a t -> int = "%identity"
val id' : ('a : any). 'a t -> int = <fun>
|}]


(***************************************************************)
(* Some primitives can't have layout_poly or any non-value jkinds *)

external[@layout_poly] dup : ('a : any). 'a -> 'a = "%obj_dup"
[%%expect{|
Line 1, characters 29-49:
1 | external[@layout_poly] dup : ('a : any). 'a -> 'a = "%obj_dup"
                                 ^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%obj_dup] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external dup : float# -> float# = "%obj_dup"
[%%expect{|
Line 1, characters 15-31:
1 | external dup : float# -> float# = "%obj_dup"
                   ^^^^^^^^^^^^^^^^
Error: The primitive [%obj_dup] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

(*************************************************************)
(* Non-explicitly quantify tvars don't work with layout_poly *)

type ('a : any) t = 'a

external[@layout_poly] id : 'a t -> 'a t = "%identity"

(* should fail as ['a] will get defaulted to [value] above without explicit
   quantification *)
let idf : ('a : float64). 'a -> 'a = id
[%%expect{|
type ('a : any) t = 'a
Line 3, characters 28-40:
3 | external[@layout_poly] id : 'a t -> 'a t = "%identity"
                                ^^^^^^^^^^^^
Error: [@layout_poly] on this external declaration has no
       effect. Consider removing it or adding a type
       variable for it to operate on.
|}]
