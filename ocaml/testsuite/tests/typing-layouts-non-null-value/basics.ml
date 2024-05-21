(* TEST
 flags = "-extension-universe alpha";
 include stdlib_stable;
 expect;
*)
type t_non_null_value : non_null_value

[%%expect{|
type t_non_null_value : non_null_value
|}]

(* [non_null_value] can be used in regular functions and modules: *)

let non_null_id (x : t_non_null_value) = x

module type S1 = sig
  val x : t_non_null_value
  val f : t_non_null_value -> t_non_null_value option
end;;

[%%expect{|
val non_null_id : t_non_null_value -> t_non_null_value = <fun>
module type S1 =
  sig
    val x : t_non_null_value
    val f : t_non_null_value -> t_non_null_value option
  end
|}]


(* [non_null_value] is a sublayout of [value]: *)

let id_value : ('a : value). 'a -> 'a = fun x -> x

let id_non_null_value : ('a : non_null_value). 'a -> 'a = fun x -> id_value x

module type S2 = sig
  type t : value
  val f : t -> t
end

module F (X : sig type t : non_null_value val f : t -> t end) : S2 = X;;

[%%expect{|
val id_value : 'a -> 'a = <fun>
val id_non_null_value : ('a : non_null_value). 'a -> 'a = <fun>
module type S2 = sig type t : value val f : t -> t end
module F : functor (X : sig type t : non_null_value val f : t -> t end) -> S2
|}]

(* [value] is not a sublayout of [non_null_value]: *)

let id_value' : ('a : value). 'a -> 'a = fun x -> id_non_null_value x;;

[%%expect{|
Line 1, characters 41-69:
1 | let id_value' : ('a : value). 'a -> 'a = fun x -> id_non_null_value x;;
                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This definition has type 'b -> 'b which is less general than
         'a. 'a -> 'a
       The layout of 'a is value, because
         of the annotation on the universal variable 'a.
       But the layout of 'a must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 3, characters 4-21.
|}]

module type S3 = sig
  type t : non_null_value
  val f : t -> t
end

module F (X : sig type t : value val f : t -> t end) : S3 = X;;

[%%expect{|
module type S3 = sig type t : non_null_value val f : t -> t end
Line 6, characters 60-61:
6 | module F (X : sig type t : value val f : t -> t end) : S3 = X;;
                                                                ^
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t val f : t -> t end
       is not included in
         S3
       Type declarations do not match:
         type t = X.t
       is not included in
         type t : non_null_value
       The layout of the first is value, because
         of the definition of t at line 6, characters 18-32.
       But the layout of the first must be a sublayout of non_null_value, because
         of the definition of t at line 2, characters 2-25.
|}]

(* Something else like [float64] is also not a sublayout of [non_null_value]: *)

let id_float_64 : ('a : float64). 'a -> 'a = fun x -> id_non_null_value x;;

[%%expect{|
Line 1, characters 72-73:
1 | let id_float_64 : ('a : float64). 'a -> 'a = fun x -> id_non_null_value x;;
                                                                            ^
Error: This expression has type ('a : float64)
       but an expression was expected of type ('b : non_null_value)
       The layout of 'a is non_null_value, because
         of the definition of id_non_null_value at line 3, characters 4-21.
       But the layout of 'a must overlap with float64, because
         of the annotation on the universal variable 'a.
|}]

module type S4 = sig
  type t : non_null_value
  val f : t -> t
end

module F (X : sig type t : float64 val f : t -> t end) : S4 = X;;

[%%expect{|
module type S4 = sig type t : non_null_value val f : t -> t end
Line 6, characters 62-63:
6 | module F (X : sig type t : float64 val f : t -> t end) : S4 = X;;
                                                                  ^
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t val f : t -> t end
       is not included in
         S4
       Type declarations do not match:
         type t = X.t
       is not included in
         type t : non_null_value
       The layout of the first is float64, because
         of the definition of t at line 6, characters 18-34.
       But the layout of the first must be a sublayout of non_null_value, because
         of the definition of t at line 2, characters 2-25.
|}]

(* Immediates are non-null: *)

let _ = id_non_null_value 3

let _ = id_non_null_value 'x'

let () = id_non_null_value ()

let _ = id_non_null_value true

let _ = id_non_null_value 3l
;;

[%%expect{|
- : int = 3
- : char = 'x'
- : bool = true
- : int32 = 3l
|}]

(* Built-in types are non-null: *)

let _ = id_non_null_value "test"

let _ = id_non_null_value [ 1; 2; 3 ]

let _ = id_non_null_value ("a", "b")

let _ = id_non_null_value None

let _ = id_non_null_value (Some 0)

let _ = id_non_null_value 3.14

let _ = id_non_null_value [| 3.; 8. |]

let _ = id_non_null_value 4L

let _ = id_non_null_value 15n

let _ = id_non_null_value Exit

let _ = id_non_null_value (Float.Array.create 2)

let _ = id_non_null_value [:0:]

let _ = id_non_null_value (Bytes.empty)
;;

[%%expect{|
- : string = "test"
- : int list = [1; 2; 3]
- : string * string = ("a", "b")
- : 'a option = None
- : int option = Some 0
- : float = 3.14
- : float array = [|3.; 8.|]
- : int64 = 4L
- : nativeint = 15n
- : exn = Stdlib.Exit
- : Float.Array.t = <abstr>
- : int iarray = [:0:]
- : bytes = Bytes.of_string ""
|}]

(* Boxed records and variants are non-null: *)

type t1 = { x : int; y : string }

type t2 = | A | B of char

let _ = id_non_null_value { x = 3; y = "test" }

let _ = id_non_null_value A

let _ = id_non_null_value (`Some_variant "foo")

let _ = ref 0
;;

[%%expect{|
type t1 = { x : int; y : string; }
type t2 = A | B of char
- : t1 = {x = 3; y = "test"}
- : t2 = A
- : [> `Some_variant of string ] = `Some_variant "foo"
- : int ref = {contents = 0}
|}]

(* Functions are non-null: *)
module M1 = struct
  [@@@warning "-5"]

  let foo = id_non_null_value (fun x -> x)

  let bar = id_non_null_value (fun (_ : float#) -> 2)
end
;;

[%%expect{|
module M1 : sig val foo : '_weak1 -> '_weak1 val bar : float# -> int end
|}]

(* First-class modules are non-null: *)

module type S1 = sig
  val bar : float# -> int
end

let _ = id_non_null_value (module M1 : S1)
;;

[%%expect{|
module type S1 = sig val bar : float# -> int end
- : (module S1) = <module>
|}]

(* CR layouts v3.0: objects should be non-null. *)

let _ = id_non_null_value (object val foo = () end)
;;

[%%expect{|
Line 1, characters 26-51:
1 | let _ = id_non_null_value (object val foo = () end)
                              ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type <  > but an expression was expected of type
         ('a : non_null_value)
       The layout of <  > is value, because
         it's the type of an object.
       But the layout of <  > must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 3, characters 4-21.
|}]

(* CR layouts v3.0: [lazy_t] possibly should be non-null. *)

let _ = id_non_null_value (lazy 3)
;;

[%%expect{|
Line 1, characters 26-34:
1 | let _ = id_non_null_value (lazy 3)
                              ^^^^^^^^
Error: This expression has type 'a lazy_t
       but an expression was expected of type ('b : non_null_value)
       The layout of 'a lazy_t is value, because
         it is the primitive value type lazy_t.
       But the layout of 'a lazy_t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 3, characters 4-21.
|}]

(* Unboxed types are not values, so they are not non-null. *)

let _ = id_non_null_value (Stdlib_stable.Float_u.of_float 3.14)
;;

[%%expect{|
Line 1, characters 26-63:
1 | let _ = id_non_null_value (Stdlib_stable.Float_u.of_float 3.14)
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type Stdlib_stable.Float_u.t = float#
       but an expression was expected of type ('a : non_null_value)
       The layout of Stdlib_stable.Float_u.t is float64, because
         it is the primitive float64 type float#.
       But the layout of Stdlib_stable.Float_u.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 3, characters 4-21.
|}]

let _ = id_non_null_value (Stdlib_stable.Int32_u.of_int 314)
;;

[%%expect{|
Line 1, characters 26-60:
1 | let _ = id_non_null_value (Stdlib_stable.Int32_u.of_int 314)
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type Stdlib_stable.Int32_u.t = int32#
       but an expression was expected of type ('a : non_null_value)
       The layout of Stdlib_stable.Int32_u.t is bits32, because
         it is the primitive bits32 type int32#.
       But the layout of Stdlib_stable.Int32_u.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 3, characters 4-21.
|}]

module Possibly_null : sig
  type t : value

  val create : int -> t
  val destroy : t -> int
end = struct
  type t = int

  let create x = x
  let destroy x = x
end
;;

[%%expect{|
module Possibly_null :
  sig type t : value val create : int -> t val destroy : t -> int end
|}]

let _ = id_non_null_value (Possibly_null.create 0)
;;

[%%expect{|
Line 1, characters 26-50:
1 | let _ = id_non_null_value (Possibly_null.create 0)
                              ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type Possibly_null.t
       but an expression was expected of type ('a : non_null_value)
       The layout of Possibly_null.t is value, because
         of the definition of t at line 2, characters 2-16.
       But the layout of Possibly_null.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 3, characters 4-21.
|}]

type 'a single_field_record = { value : 'a } [@@unboxed]
;;

type 'a single_field_variant = Value of 'a [@@unboxed]

[%%expect{|
type 'a single_field_record = { value : 'a; } [@@unboxed]
type 'a single_field_variant = Value of 'a [@@unboxed]
|}]

(* Single-field records and and variants inherit non-nullability. *)

let _ = id_non_null_value { value = 3 }

let _ = id_non_null_value { value = Possibly_null.create 1 }
;;

[%%expect{|
- : int single_field_record = {value = 3}
Line 3, characters 26-60:
3 | let _ = id_non_null_value { value = Possibly_null.create 1 }
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type Possibly_null.t single_field_record
       but an expression was expected of type ('a : non_null_value)
       The layout of Possibly_null.t single_field_record is value, because
         of the definition of t at line 2, characters 2-16.
       But the layout of Possibly_null.t single_field_record must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 3, characters 4-21.
|}]

let _ = id_non_null_value (Value "a")

let _ = id_non_null_value (Value (Possibly_null.create 1))
;;

[%%expect{|
- : string single_field_variant = <unknown constructor>
Line 3, characters 33-57:
3 | let _ = id_non_null_value (Value (Possibly_null.create 1))
                                     ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type Possibly_null.t
       but an expression was expected of type ('a : non_null_value)
       The layout of Possibly_null.t is value, because
         of the definition of t at line 2, characters 2-16.
       But the layout of Possibly_null.t must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 3, characters 4-21.
|}]

(* CR layouts v3.0: recursive single-field records should be nullable values. *)

type loopy = { field : loopy } [@@unboxed]

let rec loopy = { field = loopy }

let _ = id_non_null_value loopy
;;

[%%expect{|
Line 1, characters 15-28:
1 | type loopy = { field : loopy } [@@unboxed]
                   ^^^^^^^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of loopy is any, because
         a dummy layout of any is used to check mutually recursive datatypes.
         Please notify the Jane Street compilers group if you see this output.
       But the layout of loopy must be representable, because
         it is the type of record field field.
|}]
