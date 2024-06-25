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

type t_value : value
[%%expect {|
type t_value : value
|}]

type t : any mod global = t_value
[%%expect{|
Line 1, characters 0-33:
1 | type t : any mod global = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t_value is value, because
         of the definition of t_value at line 1, characters 0-20.
       But the layout of type t_value must be a sublayout of any, because
         of the definition of t at line 1, characters 0-33.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod unique = t_value
[%%expect{|
Line 1, characters 0-33:
1 | type t : any mod unique = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t_value is value, because
         of the definition of t_value at line 1, characters 0-20.
       But the layout of type t_value must be a sublayout of any, because
         of the definition of t at line 1, characters 0-33.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod many = t_value
[%%expect{|
Line 1, characters 0-31:
1 | type t : any mod many = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t_value is value, because
         of the definition of t_value at line 1, characters 0-20.
       But the layout of type t_value must be a sublayout of any, because
         of the definition of t at line 1, characters 0-31.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod uncontended = t_value
[%%expect{|
Line 1, characters 0-38:
1 | type t : any mod uncontended = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t_value is value, because
         of the definition of t_value at line 1, characters 0-20.
       But the layout of type t_value must be a sublayout of any, because
         of the definition of t at line 1, characters 0-38.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod portable = t_value
[%%expect{|
Line 1, characters 0-35:
1 | type t : any mod portable = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t_value is value, because
         of the definition of t_value at line 1, characters 0-20.
       But the layout of type t_value must be a sublayout of any, because
         of the definition of t at line 1, characters 0-35.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod external_ = t_value
[%%expect{|
Line 1, characters 0-36:
1 | type t : any mod external_ = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t_value is value, because
         of the definition of t_value at line 1, characters 0-20.
       But the layout of type t_value must be a sublayout of any, because
         of the definition of t at line 1, characters 0-36.
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

(************************************)
(* Test 5: Mode crossing of records *)

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

type t : any mod many = { x : string }
type t : any mod portable = { x : string }
type t : any mod uncontended = { x : string }
[%%expect {|
Line 1, characters 0-38:
1 | type t : any mod many = { x : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: This should be accepted *)

type t : any mod many = { x : t_value }
[%%expect{|
Line 1, characters 0-39:
1 | type t : any mod many = { x : t_value }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod uncontended = { x : t_value }
[%%expect{|
Line 1, characters 0-46:
1 | type t : any mod uncontended = { x : t_value }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod portable = { x : t_value }
[%%expect{|
Line 1, characters 0-43:
1 | type t : any mod portable = { x : t_value }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type u : immediate
type t : value mod portable many uncontended = { x : string; y : int; z : u }
[%%expect {|
type u : immediate
Line 2, characters 0-77:
2 | type t : value mod portable many uncontended = { x : string; y : int; z : u }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of value, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: This should be accepted *)

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

type t : any mod uncontended = { x : int }
type t : any mod portable = { x : int }
type t : any mod many = { x : int }
[%%expect{|
Line 1, characters 0-42:
1 | type t : any mod uncontended = { x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this should be accepted *)

type t : any mod global = { x : int }
[%%expect {|
Line 1, characters 0-37:
1 | type t : any mod global = { x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod external_ = { x : int }
[%%expect {|
Line 1, characters 0-40:
1 | type t : any mod external_ = { x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod unique = { x : int }
[%%expect {|
Line 1, characters 0-37:
1 | type t : any mod unique = { x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod global = { x : int } [@@unboxed]
type t : any mod portable = { x : int } [@@unboxed]
type t : any mod uncontended = { x : int } [@@unboxed]
type t : any mod external_ = { x : int } [@@unboxed]
type t : any mod many = { x : int } [@@unboxed]
type t : any mod unique = { x : int } [@@unboxed]
type t : immediate = { x : int } [@@unboxed]
[%%expect {|
type t : any mod global = { x : int; } [@@unboxed]
type t : any mod portable = { x : int; } [@@unboxed]
type t : any mod uncontended = { x : int; } [@@unboxed]
type t : any mod external_ = { x : int; } [@@unboxed]
type t : any mod many = { x : int; } [@@unboxed]
type t : any mod unique = { x : int; } [@@unboxed]
type t : immediate = { x : int; } [@@unboxed]
|}]

type ('a : immediate) t : any mod global = { x : 'a } [@@unboxed]
type ('a : immediate) t : any mod portable = { x : 'a } [@@unboxed]
type ('a : immediate) t : any mod uncontended = { x : 'a } [@@unboxed]
type ('a : immediate) t : any mod external_ = { x : 'a } [@@unboxed]
type ('a : immediate) t : any mod many = { x : 'a } [@@unboxed]
type ('a : immediate) t : any mod unique = { x : 'a } [@@unboxed]
type ('a : immediate) t : immediate = { x : 'a } [@@unboxed]
[%%expect {|
type ('a : immediate) t : any mod global = { x : 'a; } [@@unboxed]
type ('a : immediate) t : any mod portable = { x : 'a; } [@@unboxed]
type ('a : immediate) t : any mod uncontended = { x : 'a; } [@@unboxed]
type ('a : immediate) t : any mod external_ = { x : 'a; } [@@unboxed]
type ('a : immediate) t : any mod many = { x : 'a; } [@@unboxed]
type ('a : immediate) t : any mod unique = { x : 'a; } [@@unboxed]
type ('a : immediate) t : immediate = { x : 'a; } [@@unboxed]
|}]

type u : value
[%%expect {|
type u : value
|}]

type t : any mod global = { x : u } [@@unboxed]
[%%expect {|
Line 1, characters 0-47:
1 | type t : any mod global = { x : u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         of the definition of u at line 1, characters 0-14.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod portable = { x : u } [@@unboxed]
[%%expect {|
Line 1, characters 0-49:
1 | type t : any mod portable = { x : u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         of the definition of u at line 1, characters 0-14.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod uncontended = { x : u } [@@unboxed]
[%%expect {|
Line 1, characters 0-52:
1 | type t : any mod uncontended = { x : u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         of the definition of u at line 1, characters 0-14.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod external_ = { x : u } [@@unboxed]
[%%expect {|
Line 1, characters 0-50:
1 | type t : any mod external_ = { x : u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         of the definition of u at line 1, characters 0-14.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod many = { x : u } [@@unboxed]
[%%expect {|
Line 1, characters 0-45:
1 | type t : any mod many = { x : u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         of the definition of u at line 1, characters 0-14.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : any mod unique = { x : u } [@@unboxed]
[%%expect {|
Line 1, characters 0-47:
1 | type t : any mod unique = { x : u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         of the definition of u at line 1, characters 0-14.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type t : value mod global = { x : int } [@@unboxed]
let f (x : _ as (_ : immediate)) : (_ as (_ : value mod many)) = x.x
let v : (int as (_ : value mod portable)) = f { x = 5 }
[%%expect {|
type t : value mod global = { x : int; } [@@unboxed]
val f : t -> int = <fun>
val v : int = 5
|}]

type ('a : immediate) t : value mod many portable = { mutable x : 'a }
[%%expect {|
Line 1, characters 0-70:
1 | type ('a : immediate) t : value mod many portable = { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of value, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this should be accepted *)

type ('a : immediate) t : value mod global = { mutable x : 'a }
[%%expect {|
Line 1, characters 0-63:
1 | type ('a : immediate) t : value mod global = { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of value, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type ('a : immediate) t : value mod unique = { mutable x : 'a }
[%%expect {|
Line 1, characters 0-63:
1 | type ('a : immediate) t : value mod unique = { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of value, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type ('a : immediate) t : value mod uncontended = { mutable x : 'a }
[%%expect {|
Line 1, characters 0-68:
1 | type ('a : immediate) t : value mod uncontended = { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of value, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type ('a : immediate) t : value mod external_ = { mutable x : 'a }
[%%expect {|
Line 1, characters 0-66:
1 | type ('a : immediate) t : value mod external_ = { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of immediate, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type ('a : immediate) t : value mod external64 = { mutable x : 'a }
[%%expect {|
Line 1, characters 0-67:
1 | type ('a : immediate) t : value mod external64 = { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of immediate64, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

(*************************************)
(* Test 6: Mode crossing of variants *)

type t : any mod global = Foo | Bar
type t : any mod unique = Foo | Bar
type t : any mod many = Foo | Bar
type t : any mod portable = Foo | Bar
type t : any mod uncontended = Foo | Bar
type t : any mod external_ = Foo | Bar
[%%expect {|
type t = Foo | Bar
type t = Foo | Bar
type t = Foo | Bar
type t = Foo | Bar
type t = Foo | Bar
type t = Foo | Bar
|}]
(* CR layouts v2.8: These outputs should include kinds *)

type t : any mod uncontended = Foo of int | Bar
type t : any mod portable = Foo of int | Bar
type t : any mod many = Foo of int | Bar
[%%expect {|
Line 1, characters 0-47:
1 | type t : any mod uncontended = Foo of int | Bar
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed variant type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this should be accepted *)

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

type t : any mod global = Foo of int | Bar
[%%expect {|
Line 1, characters 0-42:
1 | type t : any mod global = Foo of int | Bar
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed variant type.
       But the layout of type t must be a sublayout of any, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)


type t : any mod external_ = Foo of int | Bar
[%%expect {|
Line 1, characters 0-45:
1 | type t : any mod external_ = Foo of int | Bar
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
(* CR layouts v2.8: This should be rejected once nominative types remember their kinds *)

type t : value mod global = Foo of int [@@unboxed]
type t : value mod many = Foo of int [@@unboxed]
type t : value mod unique = Foo of int [@@unboxed]
type t : value mod portable = Foo of int [@@unboxed]
type t : value mod uncontended = Foo of int [@@unboxed]
type t : value mod external_ = Foo of int [@@unboxed]
[%%expect {|
type t : value mod global = Foo of int [@@unboxed]
type t : value mod many = Foo of int [@@unboxed]
type t : value mod unique = Foo of int [@@unboxed]
type t : value mod portable = Foo of int [@@unboxed]
type t : value mod uncontended = Foo of int [@@unboxed]
type t : value mod external_ = Foo of int [@@unboxed]
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
   mismatch, not a layout mismatch. But this will be accepted once [string] mode-crosses
   portability *)

(***********************************************)
(* Test 7: Inference with modality annotations *)

type 'a t : value mod global portable uncontended many unique =
  { x : 'a @@ global portable uncontended many unique } [@@unboxed]
[%%expect {|
Lines 1-2, characters 0-67:
1 | type 'a t : value mod global portable uncontended many unique =
2 |   { x : 'a @@ global portable uncontended many unique } [@@unboxed]
Error: The layout of type t is value, because
         it instantiates an unannotated type parameter of t, defaulted to layout value.
       But the layout of type t must be a sublayout of value, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this should be accepted *)

type 'a t : value mod global portable uncontended many unique =
  Foo of 'a @@ global portable uncontended many unique [@@unboxed]
[%%expect {|
Lines 1-2, characters 0-66:
1 | type 'a t : value mod global portable uncontended many unique =
2 |   Foo of 'a @@ global portable uncontended many unique [@@unboxed]
Error: The layout of type t is value, because
         it instantiates an unannotated type parameter of t, defaulted to layout value.
       But the layout of type t must be a sublayout of value, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this should be accepted *)

type ('a : value mod global) t : value mod global = { x : 'a @@ global } [@@unboxed]
type ('a : immediate) t : immediate = { x : 'a @@ global } [@@unboxed]
type ('a : value mod global) t : value mod global = { x : 'a @@ local } [@@unboxed]
type ('a : value mod global) t : value mod global = Foo of 'a @@ global [@@unboxed]
type ('a : immediate) t : immediate = Foo of 'a @@ global [@@unboxed]
type ('a : value mod global) t : value mod global = Foo of 'a @@ local [@@unboxed]
[%%expect {|
type ('a : value mod global) t : value mod global = { global_ x : 'a; } [@@unboxed]
type ('a : immediate) t : immediate = { global_ x : 'a; } [@@unboxed]
type ('a : value mod global) t : value mod global = { x : 'a; } [@@unboxed]
type ('a : value mod global) t : value mod global = Foo of global_ 'a [@@unboxed]
type ('a : immediate) t : immediate = Foo of global_ 'a [@@unboxed]
type ('a : value mod global) t : value mod global = Foo of 'a [@@unboxed]
|}]

type ('a : value mod uncontended many) t : value mod uncontended many unique =
  { x : 'a @@ unique } [@@unboxed]
[%%expect {|
Lines 1-2, characters 0-34:
1 | type ('a : value mod uncontended many) t : value mod uncontended many unique =
2 |   { x : 'a @@ unique } [@@unboxed]
Error: The layout of type t is value, because
         of the annotation on 'a in the declaration of the type t.
       But the layout of type t must be a sublayout of value, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this should be accepted *)

type ('a : value mod external_) t : immediate =
  Foo of 'a @@ global portable uncontended many unique [@@unboxed]
[%%expect {|
Lines 1-2, characters 0-66:
1 | type ('a : value mod external_) t : immediate =
2 |   Foo of 'a @@ global portable uncontended many unique [@@unboxed]
Error: The layout of type t is immediate, because
         of the annotation on 'a in the declaration of the type t.
       But the layout of type t must be a sublayout of immediate, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this should be accepted *)

type 'a t : value mod many = { x : 'a @@ many }
type 'a t : value mod uncontended = { x : 'a @@ uncontended }
type 'a t : value mod portable = { x : 'a @@ portable }
[%%expect {|
Line 1, characters 0-47:
1 | type 'a t : value mod many = { x : 'a @@ many }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of value, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this should be accepted *)

type 'a t : value mod unique = { x : 'a @@ unique }
[%%expect {|
Line 1, characters 0-51:
1 | type 'a t : value mod unique = { x : 'a @@ unique }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of value, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

type 'a t : value mod global = { x : 'a @@ global }
[%%expect {|
Line 1, characters 0-51:
1 | type 'a t : value mod global = { x : 'a @@ global }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of value, because
         of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

(*****************************)
(* Test 8: Kind intersection *)

type ('a : value mod unique) t = ('a : value mod global)
type ('a : immediate) t = ('a : value)
type ('a : value) t = ('a : immediate)
type ('a : value mod external_ portable many) t = ('a : value mod uncontended global unique)
type ('a : value) t = ('a : any)
type ('a : value) t = ('a : value)
type ('a : bits32 mod unique) t = ('a : any mod global)
[%%expect {|
type ('a : value mod global unique) t = 'a
type ('a : immediate) t = 'a
type ('a : immediate) t = 'a
type ('a : immediate) t = 'a
type 'a t = 'a
type 'a t = 'a
type ('a : bits32 mod global unique) t = 'a
|}]

type ('a : bits32) t = ('a : word)
[%%expect {|
Line 1, characters 23-34:
1 | type ('a : bits32) t = ('a : word)
                           ^^^^^^^^^^^
Error: This type ('a : word) should be an instance of type ('a0 : bits32)
       The layout of 'a is bits32, because
         of the annotation on 'a in the declaration of the type t.
       But the layout of 'a must overlap with word, because
         of the annotation on the type variable 'a.
|}]

let f : ('a : any mod global unique) -> ('a: any mod uncontended) = fun x -> x
let f : ('a : value mod external64) -> ('a: any mod external_) = fun x -> x
let f : ('a : value) -> ('a: immediate) = fun x -> x
[%%expect {|
val f : ('a : value mod global unique uncontended). 'a -> 'a = <fun>
val f : ('a : value mod external_). 'a -> 'a = <fun>
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f : ('a : value) -> ('a: float32) = fun x -> x
[%%expect {|
Line 1, characters 29-36:
1 | let f : ('a : value) -> ('a: float32) = fun x -> x
                                 ^^^^^^^
Error: Bad layout annotation:
         The layout of 'a is value, because
           of the annotation on the type variable 'a.
         But the layout of 'a must overlap with float32, because
           of the annotation on the type variable 'a.
|}]

val x : 'a. ('a : value mod global)
[%%expect {|
Line 1, characters 8-35:
1 | val x : 'a. ('a : value mod global)
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was defaulted to have layout value.
       But it was inferred to have layout value, because
         of the annotation on the type variable 'a.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

(*****************)
(* Test 9: GADTs *)

type _ t =
  | K : (_ : value mod global) t

let f (type a : value) (x : a t) =
  let y : a @@ local = assert false in
  match x with
  | K -> y

[%%expect{|
type _ t = K : ('a : value mod global). 'a t
val f : 'a t -> 'a = <fun>
|}]

type _ t =
  | A : ('a : immediate) t
  | B : ('b : value mod portable) -> ('b : value mod unique) t
  | C : _ t

let f (type a : value) (x : a t) =
  let y : a = assert false in
  match x with
  | A ->
    let f (_ : _ as (_ : immediate)) = () in
    f y
  | B z ->
    let f : ('a : value mod portable unique). 'a -> 'a -> _ = fun _ _ -> () in
    f y z
  | C ->
    ()

[%%expect{|
type _ t =
    A : ('a : immediate). 'a t
  | B : ('b : value mod unique portable). 'b -> 'b t
  | C : 'c t
val f : 'a t -> unit = <fun>
|}]

type _ t =
  | A : ('a : immediate) t
  | B : ('b : value mod portable) -> ('b : value mod unique) t
  | C : _ t

let f (type a : value) (x : a t) =
  let y : a = assert false in
  match x with
  | A ->
    let f (_ : _ as (_ : immediate)) = () in
    f y
  | B z ->
    let f : ('a : value mod portable unique). 'a -> 'a -> _ = fun _ _ -> () in
    f y z
  | C ->
    let f (_ : _ as (_ : immediate)) = () in
    f y

[%%expect{|
type _ t =
    A : ('a : immediate). 'a t
  | B : ('b : value mod unique portable). 'b -> 'b t
  | C : 'c t
Line 17, characters 6-7:
17 |     f y
           ^
Error: This expression has type a but an expression was expected of type
         ('a : immediate)
       The layout of a is value, because
         of the annotation on the abstract type declaration for a.
       But the layout of a must be a sublayout of immediate, because
         of the definition of f at line 16, characters 10-41.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

(********************)
(* Test 10: Objects *)

type t : value = < >
[%%expect {|
type t = <  >
|}]

type t : value mod global = < >
[%%expect {|
Line 1, characters 0-31:
1 | type t : value mod global = < >
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type <  > is value, because
         it's the type of an object.
       But the layout of type <  > must be a sublayout of value, because
         of the definition of t at line 1, characters 0-31.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

let x : (_ as (_ : value)) = object end
[%%expect{|
val x : <  > = <obj>
|}]

let x : (_ as (_ : value mod global)) = object end
[%%expect {|
Line 1, characters 40-50:
1 | let x : (_ as (_ : value mod global)) = object end
                                            ^^^^^^^^^^
Error: This expression has type <  > but an expression was expected of type
         ('a : value mod global)
       The layout of <  > is value, because
         it's the type of an object.
       But the layout of <  > must be a sublayout of value, because
         of the annotation on the wildcard _ at line 1, characters 19-35.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

let x : (_ as (_ : value mod many)) = object end
[%%expect {|
Line 1, characters 38-48:
1 | let x : (_ as (_ : value mod many)) = object end
                                          ^^^^^^^^^^
Error: This expression has type <  > but an expression was expected of type
         ('a : value mod many)
       The layout of <  > is value, because
         it's the type of an object.
       But the layout of <  > must be a sublayout of value, because
         of the annotation on the wildcard _ at line 1, characters 19-33.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

let x : (_ as (_ : value mod unique)) = object end
[%%expect {|
Line 1, characters 40-50:
1 | let x : (_ as (_ : value mod unique)) = object end
                                            ^^^^^^^^^^
Error: This expression has type <  > but an expression was expected of type
         ('a : value mod unique)
       The layout of <  > is value, because
         it's the type of an object.
       But the layout of <  > must be a sublayout of value, because
         of the annotation on the wildcard _ at line 1, characters 19-35.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

let x : (_ as (_ : value mod portable)) = object end
[%%expect {|
Line 1, characters 42-52:
1 | let x : (_ as (_ : value mod portable)) = object end
                                              ^^^^^^^^^^
Error: This expression has type <  > but an expression was expected of type
         ('a : value mod portable)
       The layout of <  > is value, because
         it's the type of an object.
       But the layout of <  > must be a sublayout of value, because
         of the annotation on the wildcard _ at line 1, characters 19-37.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

let x : (_ as (_ : value mod uncontended)) = object end
[%%expect {|
Line 1, characters 45-55:
1 | let x : (_ as (_ : value mod uncontended)) = object end
                                                 ^^^^^^^^^^
Error: This expression has type <  > but an expression was expected of type
         ('a : value mod uncontended)
       The layout of <  > is value, because
         it's the type of an object.
       But the layout of <  > must be a sublayout of value, because
         of the annotation on the wildcard _ at line 1, characters 19-40.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)

let x : (_ as (_ : value mod external_)) = object end
[%%expect {|
Line 1, characters 43-53:
1 | let x : (_ as (_ : value mod external_)) = object end
                                               ^^^^^^^^^^
Error: This expression has type <  > but an expression was expected of type
         ('a : value mod external_)
       The layout of <  > is value, because
         it's the type of an object.
       But the layout of <  > must be a sublayout of immediate, because
         of the annotation on the wildcard _ at line 1, characters 19-38.
|}]
(* CR layouts v2.8: Bad error message. The error message should be about a kind or mode
   mismatch, not a layout mismatch. *)
