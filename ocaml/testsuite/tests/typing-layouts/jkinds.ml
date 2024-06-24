(* TEST
 flags = "-extension small_numbers";
 expect;
*)

(********************************)
(* Test 1: Unimplemented syntax *)

type 'a list : immutable_data with 'a

[%%expect{|
>> Fatal error: XXX unimplemented
Uncaught exception: Misc.Fatal_error

|}]

type ('a, 'b) either : immutable_data with 'a * 'b

[%%expect{|
>> Fatal error: XXX unimplemented
Uncaught exception: Misc.Fatal_error

|}]

type 'a gel : kind_of_ 'a mod global

[%%expect{|
>> Fatal error: XXX unimplemented
Uncaught exception: Misc.Fatal_error

|}]

type 'a t : _

[%%expect{|
>> Fatal error: XXX unimplemented
Uncaught exception: Misc.Fatal_error

|}]

kind_abbrev_ immediate = value mod global unique many sync uncontended

[%%expect{|
>> Fatal error: kind_abbrev not supported!
Uncaught exception: Misc.Fatal_error

|}]

kind_abbrev_ immutable_data = value mod sync uncontended many

[%%expect{|
>> Fatal error: kind_abbrev not supported!
Uncaught exception: Misc.Fatal_error

|}]

kind_abbrev_ immutable = value mod uncontended

[%%expect{|
>> Fatal error: kind_abbrev not supported!
Uncaught exception: Misc.Fatal_error

|}]

kind_abbrev_ data = value mod sync many

[%%expect{|
>> Fatal error: kind_abbrev not supported!
Uncaught exception: Misc.Fatal_error

|}]

module type S = sig
  type 'a list : immutable_data with 'a
  type ('a, 'b) either : immutable_data with 'a * 'b
  type 'a gel : kind_of_ 'a mod global
  type 'a t : _
  kind_abbrev_ immediate = value mod global unique many sync uncontended
  kind_abbrev_ immutable_data = value mod sync uncontended many
  kind_abbrev_ immutable = value mod uncontended
  kind_abbrev_ data = value mod sync many
end

[%%expect{|
>> Fatal error: XXX unimplemented
Uncaught exception: Misc.Fatal_error

|}]

(**************************************)
(* Test 2: Subkind relationships hold *)

(* type a : ka
   type b : kb = a
   typechecks iff ka <= kb *)
type a : value
type b : value = a
[%%expect{|
type a : value
type b = a
|}]

type a : value
type b : any = a
[%%expect{|
type a : value
type b = a
|}]

type a : any
type b : value = a
[%%expect{|
type a : any
Line 2, characters 0-18:
2 | type b : value = a
    ^^^^^^^^^^^^^^^^^^
Error: The layout of type a is any, because
         of the definition of a at line 1, characters 0-12.
       But the layout of type a must be a sublayout of value, because
         of the definition of b at line 2, characters 0-18.
|}]

type a : float32
type b : float32 = a
[%%expect{|
type a : float32
type b = a
|}]

type a : float32
type b : any = a
[%%expect{|
type a : float32
type b = a
|}]

type a : any
type b : float32 = a
[%%expect{|
type a : any
Line 2, characters 0-20:
2 | type b : float32 = a
    ^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is any, because
         of the definition of a at line 1, characters 0-12.
       But the layout of type a must be a sublayout of float32, because
         of the definition of b at line 2, characters 0-20.
|}]

type a : float32
type b : word = a
[%%expect{|
type a : float32
Line 2, characters 0-17:
2 | type b : word = a
    ^^^^^^^^^^^^^^^^^
Error: The layout of type a is float32, because
         of the definition of a at line 1, characters 0-16.
       But the layout of type a must be a sublayout of word, because
         of the definition of b at line 2, characters 0-17.
|}]

type a : value mod local
type b : value mod local = a
[%%expect{|
type a : value mod local
type b = a
|}]

type a : value mod global
type b : value mod local = a
[%%expect{|
type a : value mod global
type b = a
|}]

type a : value mod global
type b : value mod global = a
[%%expect{|
type a : value mod global
type b = a
|}]

type a : value mod local
type b : value mod global = a
[%%expect{|
type a : value mod local
Line 2, characters 0-29:
2 | type b : value mod global = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is value, because
         of the definition of a at line 1, characters 0-24.
       But the layout of type a must be a sublayout of value, because
         of the definition of b at line 2, characters 0-29.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type a : value mod global
type b : any mod local = a
[%%expect{|
type a : value mod global
type b = a
|}]

type a : value mod global
type b : float32 mod local = a
[%%expect{|
type a : value mod global
Line 2, characters 0-30:
2 | type b : float32 mod local = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is value, because
         of the definition of a at line 1, characters 0-25.
       But the layout of type a must be a sublayout of float32, because
         of the definition of b at line 2, characters 0-30.
|}]

type a : value mod global unique many uncontended portable external_
type b : value mod local shared once contended nonportable internal = a
[%%expect{|
type a : value mod global unique many uncontended portable external_
type b = a
|}]

type a : value mod global unique once uncontended portable external_
type b : value mod local shared many uncontended nonportable internal = a
[%%expect{|
type a : value mod global unique once uncontended portable external_
Line 2, characters 0-73:
2 | type b : value mod local shared many uncontended nonportable internal = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is immediate, because
         of the definition of a at line 1, characters 0-68.
       But the layout of type a must be a sublayout of value, because
         of the definition of b at line 2, characters 0-73.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

(********************************************************)
(* Test 3: Abbreviation primitives are properly defined *)
(* CR layouts: when we have abbreviations, these tests can become less verbose *)

type a : any
type b : any mod local shared once contended nonportable internal = a
type c : any mod local shared once contended nonportable internal
type d : any = c
[%%expect{|
type a : any
type b = a
type c : any mod local shared once contended nonportable internal
type d = c
|}]

type a : value
type b : value mod local shared once contended nonportable internal = a
type c : value mod local shared once contended nonportable internal
type d : value = c
[%%expect{|
type a : value
type b = a
type c : value mod local shared once contended nonportable internal
type d = c
|}]

type a : void
type b : void mod local shared once contended nonportable internal = a
type c : void mod local shared once contended nonportable internal
type d : void = c
[%%expect{|
Line 1, characters 9-13:
1 | type a : void
             ^^^^
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}]

type a : immediate
type b : value mod global unique many uncontended portable external_ = a
type c : value mod global unique many uncontended portable external_
type d : immediate = c
[%%expect{|
type a : immediate
type b = a
type c : value mod global unique many uncontended portable external_
type d = c
|}]

type a : immediate64
type b : value mod global unique many uncontended portable external64 = a
type c : value mod global unique many uncontended portable external64
type d : immediate64 = c
[%%expect{|
type a : immediate64
type b = a
type c : value mod global unique many uncontended portable external64
type d = c
|}]

type a : float64
type b : float64 mod global unique many uncontended portable external_ = a
type c : float64 mod global unique many uncontended portable external_
type d : float64 = c
[%%expect{|
type a : float64
type b = a
type c : float64 mod global unique many uncontended portable external_
type d = c
|}]

type a : float32
type b : float32 mod global unique many uncontended portable external_ = a
type c : float32 mod global unique many uncontended portable external_
type d : float32 = c
[%%expect{|
type a : float32
type b = a
type c : float32 mod global unique many uncontended portable external_
type d = c
|}]

type a : word
type b : word mod local shared once contended nonportable internal = a
type c : word mod local shared once contended nonportable internal
type d : word = c
[%%expect{|
type a : word
type b = a
type c : word mod local shared once contended nonportable internal
type d = c
|}]

type a : bits32
type b : bits32 mod local shared once contended nonportable internal = a
type c : bits32 mod local shared once contended nonportable internal
type d : bits32 = c
[%%expect{|
type a : bits32
type b = a
type c : bits32 mod local shared once contended nonportable internal
type d = c
|}]

type a : bits64
type b : bits64 mod local shared once contended nonportable internal = a
type c : bits64 mod local shared once contended nonportable internal
type d : bits64 = c
[%%expect{|
type a : bits64
type b = a
type c : bits64 mod local shared once contended nonportable internal
type d = c
|}]

(****************************************)
(* Test 4: Appropriate types mode cross *)

type t : any mod global unique many uncontended portable external_ = int
[%%expect{|
type t = int
|}]

type t : any mod global unique many uncontended portable external_ = float#
[%%expect{|
type t = float#
|}]

type t : any mod global unique many uncontended portable external_ = float32#
[%%expect{|
type t = float32#
|}]

type t : any mod global unique many uncontended portable external_ = int64#
[%%expect{|
type t = int64#
|}]

type t : any mod global unique many uncontended portable external_ = int32#
[%%expect{|
type t = int32#
|}]

type t : any mod global unique many uncontended portable external_ = nativeint#
[%%expect{|
type t = nativeint#
|}]

type indirect_int = int
type t : any mod global unique many uncontended portable external_ = indirect_int
[%%expect{|
type indirect_int = int
type t = indirect_int
|}]

let x : (_ : value mod uncontended) = 10
[%%expect {|
val x : int = 10
|}]

let f (x : nativeint#) =
  let _ : (_ : word mod portable many unique) = x in
  ()
[%%expect {|
val f : nativeint# -> unit = <fun>
|}]

type t : any mod global = string
[%%expect{|
Line 1, characters 0-32:
1 | type t : any mod global = string
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type string is value, because
         it is the primitive value type string.
       But the layout of type string must be a sublayout of any, because
         of the definition of t at line 1, characters 0-32.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod unique = string
[%%expect{|
Line 1, characters 0-32:
1 | type t : any mod unique = string
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type string is value, because
         it is the primitive value type string.
       But the layout of type string must be a sublayout of any, because
         of the definition of t at line 1, characters 0-32.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod many = string
[%%expect{|
Line 1, characters 0-30:
1 | type t : any mod many = string
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type string is value, because
         it is the primitive value type string.
       But the layout of type string must be a sublayout of any, because
         of the definition of t at line 1, characters 0-30.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod uncontended = string
[%%expect{|
Line 1, characters 0-37:
1 | type t : any mod uncontended = string
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type string is value, because
         it is the primitive value type string.
       But the layout of type string must be a sublayout of any, because
         of the definition of t at line 1, characters 0-37.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod portable = string
[%%expect{|
Line 1, characters 0-34:
1 | type t : any mod portable = string
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type string is value, because
         it is the primitive value type string.
       But the layout of type string must be a sublayout of any, because
         of the definition of t at line 1, characters 0-34.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod external_ = string
[%%expect{|
Line 1, characters 0-35:
1 | type t : any mod external_ = string
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type string is value, because
         it is the primitive value type string.
       But the layout of type string must be a sublayout of any, because
         of the definition of t at line 1, characters 0-35.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type ('a : value mod unique) t = { unique_field : 'a }
let x = { unique_field = "string" }
[%%expect {|
type ('a : value mod unique) t = { unique_field : 'a; }
Line 2, characters 25-33:
2 | let x = { unique_field = "string" }
                             ^^^^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : value mod unique)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of value, because
         of the definition of t at line 1, characters 0-54.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod global = { x : string }
[%%expect{|
Line 1, characters 0-40:
1 | type t : any mod global = { x : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod unique = { x : string }
[%%expect{|
Line 1, characters 0-40:
1 | type t : any mod unique = { x : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod many = { x : string }
[%%expect{|
Line 1, characters 0-38:
1 | type t : any mod many = { x : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod uncontended = { x : string }
[%%expect{|
Line 1, characters 0-45:
1 | type t : any mod uncontended = { x : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod portable = { x : string }
[%%expect{|
Line 1, characters 0-42:
1 | type t : any mod portable = { x : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod external_ = { x : string }
[%%expect{|
Line 1, characters 0-43:
1 | type t : any mod external_ = { x : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t = { x : string }
let foo : _ as (_ : value mod external_) = { x = "string" }
[%%expect {|
type t = { x : string; }
Line 2, characters 43-59:
2 | let foo : _ as (_ : value mod external_) = { x = "string" }
                                               ^^^^^^^^^^^^^^^^
Error: This expression has type t but an expression was expected of type
         ('a : value mod external_)
       The layout of t is value, because
         of the definition of t at line 1, characters 0-23.
       But the layout of t must be a sublayout of immediate, because
         of the annotation on the wildcard _ at line 2, characters 20-39.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod global = { x : int }
[%%expect{|
Line 1, characters 0-37:
1 | type t : any mod global = { x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this should be accepted *)

type t : any mod global = { x : int } [@@unboxed]
[%%expect {|
type t : any mod global = { x : int; } [@@unboxed]
|}]

type t : any mod portable = { x : int } [@@unboxed]
[%%expect {|
type t : any mod portable = { x : int; } [@@unboxed]
|}]

type t : any mod uncontended = { x : int } [@@unboxed]
[%%expect {|
type t : any mod uncontended = { x : int; } [@@unboxed]
|}]

type t : any mod external_ = { x : int } [@@unboxed]
[%%expect {|
type t : any mod external_ = { x : int; } [@@unboxed]
|}]

type t : any mod many = { x : int } [@@unboxed]
[%%expect {|
type t : any mod many = { x : int; } [@@unboxed]
|}]

type t : any mod unique = { x : int } [@@unboxed]
[%%expect {|
type t : any mod unique = { x : int; } [@@unboxed]
|}]

type t : value mod global = { x : int } [@@unboxed]
let f (x : _ as (_ : immediate)) : (_ as (_ : value mod many)) = x.x
let v : (int as (_ : value mod portable)) = f { x = 5 }
[%%expect {|
type t : value mod global = { x : int; } [@@unboxed]
val f : t -> int = <fun>
val v : int = 5
|}]

type t : any mod global = Foo | Bar
[%%expect {|
type t = Foo | Bar
|}]

type t : any mod unique = Foo of int | Bar
[%%expect {|
Line 1, characters 0-42:
1 | type t : any mod unique = Foo of int | Bar
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed variant type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod many = Foo of string | Bar
[%%expect {|
Line 1, characters 0-43:
1 | type t : any mod many = Foo of string | Bar
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed variant type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod portable = Foo of bool [@@unboxed]
let x = (Foo true : _ as (_ : value mod portable uncontended unique))
[%%expect {|
type t : any mod portable = Foo of bool [@@unboxed]
val x : t = <unknown constructor>
|}]

type t : any mod portable = Foo of string [@@unboxed]
[%%expect {|
Line 1, characters 0-53:
1 | type t : any mod portable = Foo of string [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it is the primitive value type string.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : value mod global
let g (x : t) : ('a : value mod global) = x
[%%expect{|
type t : value mod global
val g : t -> t = <fun>
|}]

type t : value mod many
let g (x : t) : ('a : value mod global) = x
[%%expect{|
type t : value mod many
Line 2, characters 42-43:
2 | let g (x : t) : ('a : value mod global) = x
                                              ^
Error: This expression has type t but an expression was expected of type
         ('a : value mod global)
       The layout of t is value, because
         of the definition of t at line 1, characters 0-23.
       But the layout of t must be a sublayout of value, because
         of the annotation on the type variable 'a.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : value mod unique
let f (x : _ as (_ : value mod unique)) = ()
let g (x : t) = f x
[%%expect {|
type t : value mod unique
val f : ('a : value mod unique). 'a -> unit = <fun>
val g : t -> unit = <fun>
|}]

type t : value mod external64
let f (x : _ as (_ : value mod unique)) = ()
let g (x : t) = f x
[%%expect {|
type t : value mod external64
val f : ('a : value mod unique). 'a -> unit = <fun>
Line 3, characters 18-19:
3 | let g (x : t) = f x
                      ^
Error: This expression has type t but an expression was expected of type
         ('a : value mod unique)
       The layout of t is immediate64, because
         of the definition of t at line 1, characters 0-29.
       But the layout of t must be a sublayout of value, because
         of the definition of f at line 2, characters 6-44.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

module A : sig
  type t : immediate
end = struct
  type t = int
end

type t : immediate = A.t

[%%expect {|
module A : sig type t : immediate end
type t = A.t
|}]

module A : sig
  type t : value
end = struct
  type t = int
end

type t : immediate = A.t

[%%expect {|
module A : sig type t : value end
Line 7, characters 0-24:
7 | type t : immediate = A.t
    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type A.t is value, because
         of the definition of t at line 2, characters 2-16.
       But the layout of type A.t must be a sublayout of immediate, because
         of the definition of t at line 7, characters 0-24.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : value = private int
let f (x : t) : _ as (_ : value mod global) = x
[%%expect {|
type t = private int
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private int
let f (x : t) : _ as (_ : value mod unique) = x
[%%expect {|
type t = private int
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private int
let f (x : t) : _ as (_ : value mod many) = x
[%%expect {|
type t = private int
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private int
let f (x : t) : _ as (_ : value mod portable) = x
[%%expect {|
type t = private int
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private int
let f (x : t) : _ as (_ : value mod uncontended) = x
[%%expect {|
type t = private int
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private int
let f (x : t) : _ as (_ : value mod external_) = x
[%%expect {|
type t = private int
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t = private int
let f (x : t) : _ as (_ : value mod global) = x
let f (x : t) : _ as (_ : value mod unique) = x
let f (x : t) : _ as (_ : value mod many) = x
let f (x : t) : _ as (_ : value mod portable) = x
let f (x : t) : _ as (_ : value mod uncontended) = x
let f (x : t) : _ as (_ : value mod external_) = x
let f (x : t) : _ as (_ : immediate) = x
[%%expect {|
type t = private int
val f : t -> t = <fun>
val f : t -> t = <fun>
val f : t -> t = <fun>
val f : t -> t = <fun>
val f : t -> t = <fun>
val f : t -> t = <fun>
val f : t -> t = <fun>
|}]

type t : value = private { x : int } [@@unboxed]
let f (x : t) : _ as (_ : value mod global) = x
[%%expect {|
type t : value = private { x : int; } [@@unboxed]
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private { x : int } [@@unboxed]
let f (x : t) : _ as (_ : value mod unique) = x
[%%expect {|
type t : value = private { x : int; } [@@unboxed]
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private { x : int } [@@unboxed]
let f (x : t) : _ as (_ : value mod many) = x
[%%expect {|
type t : value = private { x : int; } [@@unboxed]
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private { x : int } [@@unboxed]
let f (x : t) : _ as (_ : value mod portable) = x
[%%expect {|
type t : value = private { x : int; } [@@unboxed]
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private { x : int } [@@unboxed]
let f (x : t) : _ as (_ : value mod uncontended) = x
[%%expect {|
type t : value = private { x : int; } [@@unboxed]
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private { x : int } [@@unboxed]
let f (x : t) : _ as (_ : value mod external_) = x
[%%expect {|
type t : value = private { x : int; } [@@unboxed]
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

(**)
(* Test 5:  *)
