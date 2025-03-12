(* TEST
 flags = "-extension small_numbers";
 expect;
*)

(******************)
(* Test 1: Syntax *)

type 'a list : immutable_data with 'a

[%%expect{|
type 'a list : immutable_data with 'a
|}]

type ('a, 'b) either : immutable_data with 'a * 'b

[%%expect{|
type ('a, 'b) either : immutable_data with 'a with 'b
|}]

type 'a gel : kind_of_ 'a mod global

[%%expect{|
Line 1, characters 14-25:
1 | type 'a gel : kind_of_ 'a mod global
                  ^^^^^^^^^^^
Error: Unimplemented kind syntax
|}]

type 'a t : _

[%%expect{|
Line 1, characters 12-13:
1 | type 'a t : _
                ^
Error: Unimplemented kind syntax
|}]

kind_abbrev_ immediate = value mod global aliased many sync contended

[%%expect{|
>> Fatal error: kind_abbrev not supported!
Uncaught exception: Misc.Fatal_error

|}]

kind_abbrev_ immutable_data = value mod sync contended many

[%%expect{|
>> Fatal error: kind_abbrev not supported!
Uncaught exception: Misc.Fatal_error

|}]

kind_abbrev_ immutable = value mod contended

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
  kind_abbrev_ immediate = value mod global aliased many sync contended
  kind_abbrev_ immutable_data = value mod sync contended many
  kind_abbrev_ immutable = value mod contended
  kind_abbrev_ data = value mod sync many
end

[%%expect{|
Line 4, characters 16-27:
4 |   type 'a gel : kind_of_ 'a mod global
                    ^^^^^^^^^^^
Error: Unimplemented kind syntax
|}]

(**************************************)
(* Test 2: Subkind relationships hold *)

(* type a : ka
   type b : kb = a
   typechecks iff ka <= kb *)
type a : value
type b : value = a
[%%expect{|
type a
type b = a
|}]

type a : value
type b : any = a
[%%expect{|
type a
type b = a
|}]

type a : any
type b : value = a
[%%expect{|
type a : any
Line 2, characters 0-18:
2 | type b : value = a
    ^^^^^^^^^^^^^^^^^^
Error: The layout of type "a" is any
         because of the definition of a at line 1, characters 0-12.
       But the layout of type "a" must be a sublayout of value
         because of the definition of b at line 2, characters 0-18.
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
Error: The layout of type "a" is any
         because of the definition of a at line 1, characters 0-12.
       But the layout of type "a" must be a sublayout of float32
         because of the definition of b at line 2, characters 0-20.
|}]

type a : float32
type b : word = a
[%%expect{|
type a : float32
Line 2, characters 0-17:
2 | type b : word = a
    ^^^^^^^^^^^^^^^^^
Error: The layout of type "a" is float32
         because of the definition of a at line 1, characters 0-16.
       But the layout of type "a" must be a sublayout of word
         because of the definition of b at line 2, characters 0-17.
|}]

type a : value mod local
type b : value mod local = a
[%%expect{|
type a
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
type a
Line 2, characters 0-29:
2 | type b : value mod global = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "a" is value
         because of the definition of a at line 1, characters 0-24.
       But the kind of type "a" must be a subkind of value mod global
         because of the definition of b at line 2, characters 0-29.
|}]

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
Error: The layout of type "a" is value
         because of the definition of a at line 1, characters 0-25.
       But the layout of type "a" must be a sublayout of float32
         because of the definition of b at line 2, characters 0-30.
|}]

type a : value mod global aliased many contended portable external_ unyielding
type b : value mod local unique once contended nonportable internal = a
[%%expect{|
type a : immediate
type b = a
|}]

type a : value mod global aliased once contended portable external_
type b : value mod local unique many contended nonportable internal = a
[%%expect{|
type a : value mod global aliased contended portable external_
Line 2, characters 0-71:
2 | type b : value mod local unique many contended nonportable internal = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "a" is
         value mod global aliased contended portable external_
         because of the definition of a at line 1, characters 0-67.
       But the kind of type "a" must be a subkind of value mod many contended
         because of the definition of b at line 2, characters 0-71.
|}]

(********************************************************)
(* Test 3: Abbreviation primitives are properly defined *)
(* CR layouts: when we have abbreviations, these tests can become less verbose *)

type a : any
type b : any mod local unique once uncontended nonportable internal = a
type c : any mod local unique once uncontended nonportable internal
type d : any = c
[%%expect{|
type a : any
type b = a
type c : any
type d = c
|}]

type a : value
type b : value mod local unique once uncontended nonportable internal = a
type c : value mod local unique once uncontended nonportable internal
type d : value = c
[%%expect{|
type a
type b = a
type c
type d = c
|}]

type a : void
type b : void mod local unique once uncontended nonportable internal = a
type c : void mod local unique once uncontended nonportable internal
type d : void = c
[%%expect{|
Line 1, characters 9-13:
1 | type a : void
             ^^^^
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}]

type a : immediate
type b : value mod global aliased many contended portable unyielding external_ = a
type c : value mod global aliased many contended portable unyielding external_
type d : immediate = c
[%%expect{|
type a : immediate
type b = a
type c : immediate
type d = c
|}]

type a : immediate64
type b : value mod global aliased many contended portable unyielding external64 = a
type c : value mod global aliased many contended portable unyielding external64
type d : immediate64 = c
[%%expect{|
type a : immediate64
type b = a
type c : immediate64
type d = c
|}]

type a : float64
type b : float64 mod global aliased many contended portable external_ = a
type c : float64 mod global aliased many contended portable external_
type d : float64 = c
[%%expect{|
type a : float64
type b = a
type c : float64
type d = c
|}]

type a : float32
type b : float32 mod global aliased many contended portable external_ = a
type c : float32 mod global aliased many contended portable external_
type d : float32 = c
[%%expect{|
type a : float32
type b = a
type c : float32
type d = c
|}]

type a : word
type b : word mod local unique once uncontended nonportable internal = a
type c : word mod local unique once uncontended nonportable internal
type d : word = c
[%%expect{|
type a : word
type b = a
type c : word
type d = c
|}]

type a : bits32
type b : bits32 mod local unique once uncontended nonportable internal = a
type c : bits32 mod local unique once uncontended nonportable internal
type d : bits32 = c
[%%expect{|
type a : bits32
type b = a
type c : bits32
type d = c
|}]

type a : bits64
type b : bits64 mod local unique once uncontended nonportable internal = a
type c : bits64 mod local unique once uncontended nonportable internal
type d : bits64 = c
[%%expect{|
type a : bits64
type b = a
type c : bits64
type d = c
|}]

(****************************************)
(* Test 4: Appropriate types mode cross *)

type t : any mod global aliased many contended portable external_ = int
[%%expect{|
type t = int
|}]

type t : any mod global aliased many contended portable external_ = float#
[%%expect{|
type t = float#
|}]

type t : any mod global aliased many contended portable external_ = float32#
[%%expect{|
type t = float32#
|}]

type t : any mod global aliased many contended portable external_ = int64#
[%%expect{|
type t = int64#
|}]

type t : any mod global aliased many contended portable external_ = int32#
[%%expect{|
type t = int32#
|}]

type t : any mod global aliased many contended portable external_ = nativeint#
[%%expect{|
type t = nativeint#
|}]

type t : any mod global aliased many contended portable external_ = int8x16#
[%%expect{|
type t = int8x16#
|}]

type t : any mod global aliased many contended portable external_ = int16x8#
[%%expect{|
type t = int16x8#
|}]

type t : any mod global aliased many contended portable external_ = int32x4#
[%%expect{|
type t = int32x4#
|}]

type t : any mod global aliased many contended portable external_ = int64x2#
[%%expect{|
type t = int64x2#
|}]

type t : any mod global aliased many contended portable external_ = float32x4#
[%%expect{|
type t = float32x4#
|}]

type t : any mod global aliased many contended portable external_ = float64x2#
[%%expect{|
type t = float64x2#
|}]

type indirect_int = int
type t : any mod global aliased many contended portable external_ = indirect_int
[%%expect{|
type indirect_int = int
type t = indirect_int
|}]

let x : (_ : value mod contended) = 10
[%%expect {|
val x : int = 10
|}]

let f (x : nativeint#) =
  let _ : (_ : word mod portable many aliased) = x in
  ()
[%%expect {|
val f : nativeint# -> unit = <fun>
|}]

type t_value : value
[%%expect {|
type t_value
|}]

type t : any mod global = t_value
[%%expect{|
Line 1, characters 0-33:
1 | type t : any mod global = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_value" is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type "t_value" must be a subkind of any mod global
         because of the definition of t at line 1, characters 0-33.
|}]

type t : any mod aliased = t_value
[%%expect{|
Line 1, characters 0-34:
1 | type t : any mod aliased = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_value" is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type "t_value" must be a subkind of any mod aliased
         because of the definition of t at line 1, characters 0-34.
|}]

type t : any mod many = t_value
[%%expect{|
Line 1, characters 0-31:
1 | type t : any mod many = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_value" is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type "t_value" must be a subkind of any mod many
         because of the definition of t at line 1, characters 0-31.
|}]

type t : any mod contended = t_value
[%%expect{|
Line 1, characters 0-36:
1 | type t : any mod contended = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_value" is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type "t_value" must be a subkind of any mod contended
         because of the definition of t at line 1, characters 0-36.
|}]

type t : any mod portable = t_value
[%%expect{|
Line 1, characters 0-35:
1 | type t : any mod portable = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_value" is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type "t_value" must be a subkind of any mod portable
         because of the definition of t at line 1, characters 0-35.
|}]

type t : any mod external_ = t_value
[%%expect{|
Line 1, characters 0-36:
1 | type t : any mod external_ = t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_value" is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type "t_value" must be a subkind of any mod external_
         because of the definition of t at line 1, characters 0-36.
|}]

type ('a : value mod aliased) t = { aliased_field : 'a }
let x = { aliased_field = "string" }
[%%expect {|
type ('a : value mod aliased) t = { aliased_field : 'a; }
Line 2, characters 26-34:
2 | let x = { aliased_field = "string" }
                              ^^^^^^^^
Error: This expression has type "string" but an expression was expected of type
         "('a : value mod aliased)"
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of value mod aliased
         because of the definition of t at line 1, characters 0-56.
|}]

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
Error: This expression has type "t" but an expression was expected of type
         "('a : value mod global)"
       The kind of t is value mod many
         because of the definition of t at line 1, characters 0-23.
       But the kind of t must be a subkind of value mod global
         because of the annotation on the type variable 'a.
|}]

type t : value mod aliased
let f (x : _ as (_ : value mod aliased)) = ()
let g (x : t) = f x
[%%expect {|
type t : value mod aliased
val f : ('a : value mod aliased). 'a -> unit = <fun>
val g : t -> unit = <fun>
|}]

type t : value mod external64
let f (x : _ as (_ : value mod aliased)) = ()
let g (x : t) = f x
[%%expect {|
type t : value mod external64
val f : ('a : value mod aliased). 'a -> unit = <fun>
Line 3, characters 18-19:
3 | let g (x : t) = f x
                      ^
Error: This expression has type "t" but an expression was expected of type
         "('a : value mod aliased)"
       The kind of t is value mod external64
         because of the definition of t at line 1, characters 0-29.
       But the kind of t must be a subkind of value mod aliased
         because of the definition of f at line 2, characters 6-45.
|}]

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
module A : sig type t end
Line 7, characters 0-24:
7 | type t : immediate = A.t
    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "A.t" is value
         because of the definition of t at line 2, characters 2-16.
       But the kind of type "A.t" must be a subkind of immediate
         because of the definition of t at line 7, characters 0-24.
|}]

type t : value = private int
let f (x : t) : _ as (_ : value mod global) = x
[%%expect {|
type t = private int
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private int
let f (x : t) : _ as (_ : value mod aliased) = x
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
let f (x : t) : _ as (_ : value mod contended) = x
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
let f (x : t) : _ as (_ : value mod aliased) = x
let f (x : t) : _ as (_ : value mod many) = x
let f (x : t) : _ as (_ : value mod portable) = x
let f (x : t) : _ as (_ : value mod contended) = x
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
type t = private { x : int; } [@@unboxed]
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private { x : int } [@@unboxed]
let f (x : t) : _ as (_ : value mod aliased) = x
[%%expect {|
type t = private { x : int; } [@@unboxed]
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private { x : int } [@@unboxed]
let f (x : t) : _ as (_ : value mod many) = x
[%%expect {|
type t = private { x : int; } [@@unboxed]
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private { x : int } [@@unboxed]
let f (x : t) : _ as (_ : value mod portable) = x
[%%expect {|
type t = private { x : int; } [@@unboxed]
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private { x : int } [@@unboxed]
let f (x : t) : _ as (_ : value mod contended) = x
[%%expect {|
type t = private { x : int; } [@@unboxed]
val f : t -> t = <fun>
|}]
(* CR layouts v2.8: This should fail since t is nominative *)

type t : value = private { x : int } [@@unboxed]
let f (x : t) : _ as (_ : value mod external_) = x
[%%expect {|
type t = private { x : int; } [@@unboxed]
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
Error: The kind of type "t" is immutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of any mod global
         because of the annotation on the declaration of the type t.
|}]

type t : any mod aliased = { x : string }
[%%expect{|
Line 1, characters 0-41:
1 | type t : any mod aliased = { x : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of any mod aliased
         because of the annotation on the declaration of the type t.
|}]

type t : any mod external_ = { x : string }
[%%expect{|
Line 1, characters 0-43:
1 | type t : any mod external_ = { x : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of any mod external_
         because of the annotation on the declaration of the type t.
|}]

type t : any mod many = { x : string }
type t : any mod portable = { x : string }
type t : any mod contended = { x : string }
[%%expect {|
type t = { x : string; }
type t = { x : string; }
type t = { x : string; }
|}]

type t : any mod many = { x : t_value }
[%%expect{|
Line 1, characters 0-39:
1 | type t : any mod many = { x : t_value }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with t_value
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of any mod many
         because of the annotation on the declaration of the type t.
|}]

type t : any mod contended = { x : t_value }
[%%expect{|
Line 1, characters 0-44:
1 | type t : any mod contended = { x : t_value }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with t_value
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of any mod contended
         because of the annotation on the declaration of the type t.
|}]

type t : any mod portable = { x : t_value }
[%%expect{|
Line 1, characters 0-43:
1 | type t : any mod portable = { x : t_value }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with t_value
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of any mod portable
         because of the annotation on the declaration of the type t.
|}]

type t : any mod many contended portable global = { x : t_value }
[%%expect{|
Line 1, characters 0-65:
1 | type t : any mod many contended portable global = { x : t_value }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with t_value
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of
         any mod global many contended portable
         because of the annotation on the declaration of the type t.
|}]

type u : immediate
type t : value mod portable many contended = { x : string; y : int; z : u }
[%%expect {|
type u : immediate
type t = { x : string; y : int; z : u; }
|}]

type t = { x : string }
let foo : _ as (_ : value mod external_) = { x = "string" }
[%%expect {|
type t = { x : string; }
Line 2, characters 43-59:
2 | let foo : _ as (_ : value mod external_) = { x = "string" }
                                               ^^^^^^^^^^^^^^^^
Error: This expression has type "t" but an expression was expected of type
         "('a : value mod external_)"
       The kind of t is immutable_data
         because of the definition of t at line 1, characters 0-23.
       But the kind of t must be a subkind of value mod external_
         because of the annotation on the wildcard _ at line 2, characters 20-39.
|}]

type t : any mod contended = { x : int }
type t : any mod portable = { x : int }
type t : any mod many = { x : int }
[%%expect{|
type t = { x : int; }
type t = { x : int; }
type t = { x : int; }
|}]

type t : any mod global = { x : int }
[%%expect {|
Line 1, characters 0-37:
1 | type t : any mod global = { x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of any mod global
         because of the annotation on the declaration of the type t.
|}]

type t : any mod external_ = { x : int }
[%%expect {|
Line 1, characters 0-40:
1 | type t : any mod external_ = { x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of any mod external_
         because of the annotation on the declaration of the type t.
|}]

type t : any mod aliased = { x : int }
[%%expect {|
Line 1, characters 0-38:
1 | type t : any mod aliased = { x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of any mod aliased
         because of the annotation on the declaration of the type t.
|}]

type t : any mod global = { x : int } [@@unboxed]
type t : any mod portable = { x : int } [@@unboxed]
type t : any mod contended = { x : int } [@@unboxed]
type t : any mod external_ = { x : int } [@@unboxed]
type t : any mod many = { x : int } [@@unboxed]
type t : any mod aliased = { x : int } [@@unboxed]
type t : immediate = { x : int } [@@unboxed]
[%%expect {|
type t = { x : int; } [@@unboxed]
type t = { x : int; } [@@unboxed]
type t = { x : int; } [@@unboxed]
type t = { x : int; } [@@unboxed]
type t = { x : int; } [@@unboxed]
type t = { x : int; } [@@unboxed]
type t = { x : int; } [@@unboxed]
|}]

type ('a : immediate) t : any mod global = { x : 'a } [@@unboxed]
type ('a : immediate) t : any mod portable = { x : 'a } [@@unboxed]
type ('a : immediate) t : any mod contended = { x : 'a } [@@unboxed]
type ('a : immediate) t : any mod external_ = { x : 'a } [@@unboxed]
type ('a : immediate) t : any mod many = { x : 'a } [@@unboxed]
type ('a : immediate) t : any mod aliased = { x : 'a } [@@unboxed]
type ('a : immediate) t : immediate = { x : 'a } [@@unboxed]
[%%expect {|
type ('a : immediate) t = { x : 'a; } [@@unboxed]
type ('a : immediate) t = { x : 'a; } [@@unboxed]
type ('a : immediate) t = { x : 'a; } [@@unboxed]
type ('a : immediate) t = { x : 'a; } [@@unboxed]
type ('a : immediate) t = { x : 'a; } [@@unboxed]
type ('a : immediate) t = { x : 'a; } [@@unboxed]
type ('a : immediate) t = { x : 'a; } [@@unboxed]
|}]

type u : value
[%%expect {|
type u
|}]

type t : any mod global = { x : u } [@@unboxed]
[%%expect {|
Line 1, characters 0-47:
1 | type t : any mod global = { x : u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value
         because of the definition of u at line 1, characters 0-14.
       But the kind of type "t" must be a subkind of any mod global
         because of the annotation on the declaration of the type t.
|}]

type t : any mod portable = { x : u } [@@unboxed]
[%%expect {|
Line 1, characters 0-49:
1 | type t : any mod portable = { x : u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value
         because of the definition of u at line 1, characters 0-14.
       But the kind of type "t" must be a subkind of any mod portable
         because of the annotation on the declaration of the type t.
|}]

type t : any mod contended = { x : u } [@@unboxed]
[%%expect {|
Line 1, characters 0-50:
1 | type t : any mod contended = { x : u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value
         because of the definition of u at line 1, characters 0-14.
       But the kind of type "t" must be a subkind of any mod contended
         because of the annotation on the declaration of the type t.
|}]

type t : any mod external_ = { x : u } [@@unboxed]
[%%expect {|
Line 1, characters 0-50:
1 | type t : any mod external_ = { x : u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value
         because of the definition of u at line 1, characters 0-14.
       But the kind of type "t" must be a subkind of any mod external_
         because of the annotation on the declaration of the type t.
|}]

type t : any mod many = { x : u } [@@unboxed]
[%%expect {|
Line 1, characters 0-45:
1 | type t : any mod many = { x : u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value
         because of the definition of u at line 1, characters 0-14.
       But the kind of type "t" must be a subkind of any mod many
         because of the annotation on the declaration of the type t.
|}]

type t : any mod aliased = { x : u } [@@unboxed]
[%%expect {|
Line 1, characters 0-48:
1 | type t : any mod aliased = { x : u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value
         because of the definition of u at line 1, characters 0-14.
       But the kind of type "t" must be a subkind of any mod aliased
         because of the annotation on the declaration of the type t.
|}]

type t : value mod global = { x : int } [@@unboxed]
let f (x : _ as (_ : immediate)) : (_ as (_ : value mod many)) = x.x
let v : (int as (_ : value mod portable)) = f { x = 5 }
[%%expect {|
type t = { x : int; } [@@unboxed]
val f : t -> int = <fun>
val v : int = 5
|}]

type ('a : immediate) t : value mod many portable = { mutable x : 'a }
[%%expect {|
type ('a : immediate) t = { mutable x : 'a; }
|}]

type ('a : immediate) t : value mod global = { mutable x : 'a }
[%%expect {|
Line 1, characters 0-63:
1 | type ('a : immediate) t : value mod global = { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data with 'a @@ many
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod global
         because of the annotation on the declaration of the type t.

       The first mode-crosses less than the second along:
         locality: mod local ≰ mod global
|}]

type ('a : immediate) t : value mod aliased = { mutable x : 'a }
[%%expect {|
Line 1, characters 0-64:
1 | type ('a : immediate) t : value mod aliased = { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data with 'a @@ many
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod aliased
         because of the annotation on the declaration of the type t.

       The first mode-crosses less than the second along:
         uniqueness: mod unique ≰ mod aliased
|}]

type ('a : immediate) t : value mod contended = { mutable x : 'a }
[%%expect {|
Line 1, characters 0-66:
1 | type ('a : immediate) t : value mod contended = { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data with 'a @@ many
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod contended
         because of the annotation on the declaration of the type t.

       The first mode-crosses less than the second along:
         contention: mod uncontended ≰ mod contended
|}]

type ('a : immediate) t : value mod external_ = { mutable x : 'a }
[%%expect {|
Line 1, characters 0-66:
1 | type ('a : immediate) t : value mod external_ = { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data with 'a @@ many
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod external_
         because of the annotation on the declaration of the type t.

       The first mode-crosses less than the second along:
         externality: mod internal ≰ mod external_
|}]

type ('a : immediate) t : value mod external64 = { mutable x : 'a }
[%%expect {|
Line 1, characters 0-67:
1 | type ('a : immediate) t : value mod external64 = { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data with 'a @@ many
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod external64
         because of the annotation on the declaration of the type t.

       The first mode-crosses less than the second along:
         externality: mod internal ≰ mod external64
|}]

(*************************************)
(* Test 6: Mode crossing of variants *)

type t : any mod global = Foo | Bar
type t : any mod aliased = Foo | Bar
type t : any mod many = Foo | Bar
type t : any mod portable = Foo | Bar
type t : any mod contended = Foo | Bar
type t : any mod external_ = Foo | Bar
[%%expect {|
type t = Foo | Bar
type t = Foo | Bar
type t = Foo | Bar
type t = Foo | Bar
type t = Foo | Bar
type t = Foo | Bar
|}]

type t : any mod contended = Foo of int | Bar
type t : any mod portable = Foo of int | Bar
type t : any mod many = Foo of int | Bar
[%%expect {|
type t = Foo of int | Bar
type t = Foo of int | Bar
type t = Foo of int | Bar
|}]

type t : any mod aliased = Foo of int | Bar
[%%expect {|
Line 1, characters 0-43:
1 | type t : any mod aliased = Foo of int | Bar
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of any mod aliased
         because of the annotation on the declaration of the type t.
|}]

type t : any mod global = Foo of int | Bar
[%%expect {|
Line 1, characters 0-42:
1 | type t : any mod global = Foo of int | Bar
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of any mod global
         because of the annotation on the declaration of the type t.
|}]


type t : any mod external_ = Foo of int | Bar
[%%expect {|
Line 1, characters 0-45:
1 | type t : any mod external_ = Foo of int | Bar
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of any mod external_
         because of the annotation on the declaration of the type t.
|}]

type t : any mod portable = Foo of bool [@@unboxed]
let x = (Foo true : _ as (_ : value mod portable contended aliased))
[%%expect {|
type t = Foo of bool [@@unboxed]
val x : t = <unknown constructor>
|}]
(* CR layouts v2.8: This should be rejected once nominative types remember their kinds *)

type t : value mod global = Foo of int [@@unboxed]
type t : value mod many = Foo of int [@@unboxed]
type t : value mod aliased = Foo of int [@@unboxed]
type t : value mod portable = Foo of int [@@unboxed]
type t : value mod contended = Foo of int [@@unboxed]
type t : value mod external_ = Foo of int [@@unboxed]
[%%expect {|
type t = Foo of int [@@unboxed]
type t = Foo of int [@@unboxed]
type t = Foo of int [@@unboxed]
type t = Foo of int [@@unboxed]
type t = Foo of int [@@unboxed]
type t = Foo of int [@@unboxed]
|}]

type t : any mod portable = Foo of t_value [@@unboxed]
[%%expect {|
Line 1, characters 0-54:
1 | type t : any mod portable = Foo of t_value [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type "t" must be a subkind of any mod portable
         because of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this should be accepted portability *)

(***********************************************)
(* Test 7: Inference with modality annotations *)

type 'a t : value mod global portable contended many aliased unyielding =
  { x : 'a @@ global portable contended many aliased } [@@unboxed]
[%%expect {|
Lines 1-2, characters 0-66:
1 | type 'a t : value mod global portable contended many aliased unyielding =
2 |   { x : 'a @@ global portable contended many aliased } [@@unboxed]
Error: The kind of type "t" is value mod global aliased many contended portable
         because it instantiates an unannotated type parameter of t,
         chosen to have kind value mod global aliased many contended portable.
       But the kind of type "t" must be a subkind of
         immutable_data mod global aliased
         because of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this could be accepted, if we infer ('a : value mod
   unyielding). We do not currently do this, because we finish inference of the
   type declaration before ever consulting the jkind annotation. *)
(* CR layouts v2.8: In addition, the error message is a little sad, in that it
   reports the jkind of t imprecisely. Really, its jkind should have "mod
   unyielding with 'a @@ stuff" -- because if 'a mode-crossing yielding, then so
   does 'a t (and this is true in practice). What's going on here is that the
   algorithm in typedecl uses the jkind of 'a (which is value) as the jkind of
   'a t (after taking modalities into account). This is misleading, though
   understandable. In the end, though, this bug manifests only as a confusing
   error message, not deeper misbehavior, and so is low priority. When we have
   [layout_of], we'll be able to give a better jkind to [@@unboxed] types, and
   this will likely improve. *)

type 'a t : value mod global portable contended many aliased unyielding =
  Foo of 'a @@ global portable contended many aliased [@@unboxed]
[%%expect {|
Lines 1-2, characters 0-65:
1 | type 'a t : value mod global portable contended many aliased unyielding =
2 |   Foo of 'a @@ global portable contended many aliased [@@unboxed]
Error: The kind of type "t" is value
         because it instantiates an unannotated type parameter of t,
         chosen to have kind value.
       But the kind of type "t" must be a subkind of
         immutable_data mod global aliased
         because of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this should be accepted *)

type ('a : value mod global) t : value mod global = { x : 'a @@ global } [@@unboxed]
type ('a : immediate) t : immediate = { x : 'a @@ global } [@@unboxed]
type ('a : value mod global) t : value mod global = { x : 'a @@ local } [@@unboxed]
type ('a : value mod global) t : value mod global = Foo of 'a @@ global [@@unboxed]
type ('a : immediate) t : immediate = Foo of 'a @@ global [@@unboxed]
type ('a : value mod global) t : value mod global = Foo of 'a @@ local [@@unboxed]
[%%expect {|
type ('a : value mod global) t = { global_ x : 'a; } [@@unboxed]
type ('a : immediate) t = { global_ x : 'a; } [@@unboxed]
type ('a : value mod global) t = { x : 'a; } [@@unboxed]
type ('a : value mod global) t = Foo of global_ 'a [@@unboxed]
type ('a : immediate) t = Foo of global_ 'a [@@unboxed]
type ('a : value mod global) t = Foo of 'a [@@unboxed]
|}]

type ('a : value mod contended many) t : value mod contended many aliased =
  { x : 'a @@ aliased } [@@unboxed]
[%%expect {|
type ('a : value mod many contended) t = { x : 'a @@ aliased; } [@@unboxed]
|}]

type ('a : value mod external_) t : immediate =
  Foo of 'a @@ global portable contended many aliased [@@unboxed]
[%%expect {|
Lines 1-2, characters 0-65:
1 | type ('a : value mod external_) t : immediate =
2 |   Foo of 'a @@ global portable contended many aliased [@@unboxed]
Error: The kind of type "t" is value mod external_
         because of the annotation on 'a in the declaration of the type t.
       But the kind of type "t" must be a subkind of immediate
         because of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this should be accepted *)

type 'a t : value mod many = { x : 'a @@ many }
type 'a t : value mod contended = { x : 'a @@ contended }
type 'a t : value mod portable = { x : 'a @@ portable }
[%%expect {|
type 'a t = { x : 'a @@ many; }
type 'a t = { x : 'a @@ contended; }
type 'a t = { x : 'a @@ portable; }
|}]

type 'a t : immutable_data with 'a @@ many = { x : 'a @@ many }
type 'a t : immutable_data with 'a @@ contended = { x : 'a @@ contended }
type 'a t : immutable_data with 'a @@ portable = { x : 'a @@ portable }
[%%expect{|
type 'a t = { x : 'a @@ many; }
type 'a t = { x : 'a @@ contended; }
type 'a t = { x : 'a @@ portable; }
|}]

type 'a t : value mod aliased = { x : 'a @@ aliased }
[%%expect {|
Line 1, characters 0-53:
1 | type 'a t : value mod aliased = { x : 'a @@ aliased }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod aliased
         because of the annotation on the declaration of the type t.
|}]

type 'a t : value mod global = { x : 'a @@ global }
[%%expect {|
Line 1, characters 0-51:
1 | type 'a t : value mod global = { x : 'a @@ global }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod global
         because of the annotation on the declaration of the type t.
|}]

(*****************************)
(* Test 8: Kind intersection *)

type ('a : value mod aliased) t = ('a : value mod global)
type ('a : immediate) t = ('a : value)
type ('a : value) t = ('a : immediate)
type ('a : value mod external_ portable many unyielding) t = ('a : value mod contended global aliased)
type ('a : value) t = ('a : any)
type ('a : value) t = ('a : value)
type ('a : bits32 mod aliased) t = ('a : any mod global)
[%%expect {|
type ('a : value mod global aliased) t = 'a
type ('a : immediate) t = 'a
type ('a : immediate) t = 'a
type ('a : immediate) t = 'a
type 'a t = 'a
type 'a t = 'a
type ('a : bits32) t = 'a
|}]

type ('a : bits32) t = ('a : word)
[%%expect {|
Line 1, characters 29-33:
1 | type ('a : bits32) t = ('a : word)
                                 ^^^^
Error: Bad layout annotation:
         The layout of "'a" is bits32
           because of the annotation on 'a in the declaration of the type t.
         But the layout of "'a" must overlap with word
           because of the annotation on the type variable 'a.
|}]

let f : ('a : any mod global aliased) -> ('a: any mod contended) = fun x -> x
let f : ('a : value mod external64) -> ('a: any mod external_) = fun x -> x
let f : ('a : value) -> ('a: immediate) = fun x -> x
[%%expect {|
val f : ('a : value_or_null mod global aliased contended). 'a -> 'a = <fun>
val f : ('a : value mod external_). 'a -> 'a = <fun>
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f : ('a : value) -> ('a: float32) = fun x -> x
[%%expect {|
Line 1, characters 29-36:
1 | let f : ('a : value) -> ('a: float32) = fun x -> x
                                 ^^^^^^^
Error: Bad layout annotation:
         The layout of "'a" is value
           because of the annotation on the type variable 'a.
         But the layout of "'a" must overlap with float32
           because of the annotation on the type variable 'a.
|}]

val x : 'a. ('a : value mod global)
[%%expect {|
Line 1, characters 8-35:
1 | val x : 'a. ('a : value mod global)
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was defaulted to have kind value.
       But it was inferred to have kind value mod global
         because of the annotation on the type variable 'a.
|}]

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
  | B : ('b : value mod portable) -> ('b : value mod aliased) t
  | C : _ t

let f (type a : value) (x : a t) =
  let y : a = assert false in
  match x with
  | A ->
    let f (_ : _ as (_ : immediate)) = () in
    f y
  | B z ->
    let f : ('a : value mod portable aliased). 'a -> 'a -> _ = fun _ _ -> () in
    f y z
  | C ->
    ()

[%%expect{|
type _ t =
    A : ('a : immediate). 'a t
  | B : ('b : value mod aliased portable). 'b -> 'b t
  | C : 'c t
val f : 'a t -> unit = <fun>
|}]

type _ t =
  | A : ('a : immediate) t
  | B : ('b : value mod portable) -> ('b : value mod aliased) t
  | C : _ t

let f (type a : value) (x : a t) =
  let y : a = assert false in
  match x with
  | A ->
    let f (_ : _ as (_ : immediate)) = () in
    f y
  | B z ->
    let f : ('a : value mod portable aliased). 'a -> 'a -> _ = fun _ _ -> () in
    f y z
  | C ->
    let f (_ : _ as (_ : immediate)) = () in
    f y

[%%expect{|
type _ t =
    A : ('a : immediate). 'a t
  | B : ('b : value mod aliased portable). 'b -> 'b t
  | C : 'c t
Line 17, characters 6-7:
17 |     f y
           ^
Error: This expression has type "a" but an expression was expected of type
         "('a : immediate)"
       The kind of a is value
         because of the annotation on the abstract type declaration for a.
       But the kind of a must be a subkind of immediate
         because of the definition of f at line 16, characters 10-41.
|}]

(********************)
(* Test 10: Objects *)

type t : value = < >
[%%expect {|
type t = <  >
|}]

type t : value mod global = < >
[%%expect {|
type t = <  >
|}]

let x : (_ as (_ : value)) = object end
[%%expect{|
val x : <  > = <obj>
|}]

let x : (_ as (_ : value mod global)) = object end
[%%expect {|
val x : <  > = <obj>
|}]

let x : (_ as (_ : value mod many)) = object end
[%%expect {|
val x : <  > = <obj>
|}]

let x : (_ as (_ : value mod aliased)) = object end
[%%expect {|
Line 1, characters 41-51:
1 | let x : (_ as (_ : value mod aliased)) = object end
                                             ^^^^^^^^^^
Error: This expression has type "<  >" but an expression was expected of type
         "('a : value mod aliased)"
       The kind of <  > is value mod global many unyielding
         because it's the type of an object.
       But the kind of <  > must be a subkind of value mod aliased
         because of the annotation on the wildcard _ at line 1, characters 19-36.
|}]

let x : (_ as (_ : value mod portable)) = object end
[%%expect {|
Line 1, characters 42-52:
1 | let x : (_ as (_ : value mod portable)) = object end
                                              ^^^^^^^^^^
Error: This expression has type "<  >" but an expression was expected of type
         "('a : value mod portable)"
       The kind of <  > is value mod global many unyielding
         because it's the type of an object.
       But the kind of <  > must be a subkind of value mod portable
         because of the annotation on the wildcard _ at line 1, characters 19-37.
|}]

let x : (_ as (_ : value mod contended)) = object end
[%%expect {|
Line 1, characters 43-53:
1 | let x : (_ as (_ : value mod contended)) = object end
                                               ^^^^^^^^^^
Error: This expression has type "<  >" but an expression was expected of type
         "('a : value mod contended)"
       The kind of <  > is value mod global many unyielding
         because it's the type of an object.
       But the kind of <  > must be a subkind of value mod contended
         because of the annotation on the wildcard _ at line 1, characters 19-38.
|}]

let x : (_ as (_ : value mod external_)) = object end
[%%expect {|
Line 1, characters 43-53:
1 | let x : (_ as (_ : value mod external_)) = object end
                                               ^^^^^^^^^^
Error: This expression has type "<  >" but an expression was expected of type
         "('a : value mod external_)"
       The kind of <  > is value mod global many unyielding
         because it's the type of an object.
       But the kind of <  > must be a subkind of value mod external_
         because of the annotation on the wildcard _ at line 1, characters 19-38.
|}]

(****************************************)
(* Test 11: Inference of type parameter *)

type 'a t : any = 'a
[%%expect {|
type 'a t = 'a
|}]

type 'a t : value = 'a
[%%expect {|
type 'a t = 'a
|}]

type 'a t : value mod global = 'a
[%%expect {|
type ('a : value mod global) t = 'a
|}]

type 'a t : word = 'a
[%%expect {|
Line 1, characters 0-21:
1 | type 'a t : word = 'a
    ^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "'a" is value
         because of the definition of t at line 1, characters 0-21.
       But the layout of type "'a" must overlap with word
         because of the definition of t at line 1, characters 0-21.
|}]
(* CR layouts v2.8: this should be accepted; 'a should be inferred to have kind
   word *)

type 'a t : any = private 'a
[%%expect {|
type 'a t = private 'a
|}]

type 'a t : value = private 'a
[%%expect {|
type 'a t = private 'a
|}]

type 'a t : value mod global = private 'a
[%%expect {|
type ('a : value mod global) t = private 'a
|}]

type 'a t : word = private 'a
[%%expect {|
Line 1, characters 0-29:
1 | type 'a t : word = private 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "'a" is value
         because of the definition of t at line 1, characters 0-29.
       But the layout of type "'a" must overlap with word
         because of the definition of t at line 1, characters 0-29.
|}]
(* CR layouts v2.8: this should be accepted; 'a should be inferred to have kind
  word *)

type 'a t : value mod global = Foo of 'a [@@unboxed]
[%%expect {|
Line 1, characters 0-52:
1 | type 'a t : value mod global = Foo of 'a [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value
         because it instantiates an unannotated type parameter of t,
         chosen to have kind value.
       But the kind of type "t" must be a subkind of value mod global
         because of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this should be accepted; 'a should be inferred to have kind
  value mod global *)

type 'a t : value mod global = { x : 'a }
[%%expect {|
Line 1, characters 0-41:
1 | type 'a t : value mod global = { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod global
         because of the annotation on the declaration of the type t.
|}]

type 'a t : value mod many = { x : 'a }
[%%expect {|
Line 1, characters 0-39:
1 | type 'a t : value mod many = { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod many
         because of the annotation on the declaration of the type t.
|}]

(*************************************)
(* Test 12: Bug in check_constraints *)

(* This requires the [Ctype.instance] call in [check_constraints_rec]. *)
type 'a u
type 'a t =
  | None
  | Some of ('a * 'a) t u

[%%expect{|
type 'a u
type 'a t = None | Some of ('a * 'a) t u
|}]

(*********************************)
(* Test 13: Bug in class methods *)

type t =
  | Atom of string
  | List of t list

class type sexp_of =
    object
      method array : ('a -> t) -> ('a array -> t)
    end


[%%expect{|
type t = Atom of string | List of t list
class type sexp_of = object method array : ('a -> t) -> 'a array -> t end
|}]

(*******************************)
(* Test 14: Bug in rec modules *)
module rec Gadt_option : sig
  type 'a t = T : 'a option -> 'a t [@@unboxed]
end = Gadt_option

and Also_gadt_option : sig
  type 'a t = 'a Gadt_option.t
end = struct
  type 'a t = 'a Gadt_option.t
end
[%%expect {|
module rec Gadt_option :
  sig type 'a t = T : 'a option -> 'a t [@@unboxed] end
and Also_gadt_option : sig type 'a t = 'a Gadt_option.t end
|}]

(*********************************)
(* Test 15: principality *)

let id x = x
let require_portable (_ : ('a : value mod portable)) = ()
type 'a t = Box of 'a

let f x =
  match true with
  | true ->
    let _ : int = x in
    ()
  | false ->
    (Box x)
     |> id
     |> require_portable
[%%expect {|
val id : 'a -> 'a = <fun>
val require_portable : ('a : value mod portable). 'a -> unit = <fun>
type 'a t = Box of 'a
val f : int -> unit = <fun>
|}, Principal{|
val id : 'a -> 'a = <fun>
val require_portable : ('a : value mod portable). 'a -> unit = <fun>
type 'a t = Box of 'a
Lines 11-12, characters 4-10:
11 | ....(Box x)
12 |      |> id
Error: This expression has type "int t" but an expression was expected of type
         "('a : value mod portable)"
       The kind of int t is immutable_data with int
         because of the definition of t at line 3, characters 0-21.
       But the kind of int t must be a subkind of value mod portable
         because of the definition of require_portable at line 2, characters 21-57.
|}]

(*********************************)
(* Test 15: extensible variants *)

(* The best kind an extensible variant can get is [value] *)
type extensible : value = ..
[%%expect{|
type extensible = ..
|}]

(* Since the kind is [best], it should normalize away *)
module M : sig
  type t : immediate with extensible
end = struct
  type t
end
[%%expect{|
module M : sig type t end
|}]

(*********************************)
(* Test 16: principality *)

let id x = x
let require_portable (_ : ('a : value mod portable)) = ()
type 'a t = Box of 'a

let f x =
  match true with
  | true ->
    let _ : int = x in
    ()
  | false ->
    (Box x)
     |> id
     |> require_portable
[%%expect {|
val id : 'a -> 'a = <fun>
val require_portable : ('a : value mod portable). 'a -> unit = <fun>
type 'a t = Box of 'a
val f : int -> unit = <fun>
|}, Principal{|
val id : 'a -> 'a = <fun>
val require_portable : ('a : value mod portable). 'a -> unit = <fun>
type 'a t = Box of 'a
Lines 11-12, characters 4-10:
11 | ....(Box x)
12 |      |> id
Error: This expression has type "int t" but an expression was expected of type
         "('a : value mod portable)"
       The kind of int t is immutable_data with int
         because of the definition of t at line 3, characters 0-21.
       But the kind of int t must be a subkind of value mod portable
         because of the definition of require_portable at line 2, characters 21-57.
|}]

(*********************************)
(* Test 17: extensible variants *)

(* The best kind an extensible variant can get is [value] *)
type extensible : value = ..
[%%expect{|
type extensible = ..
|}]

(* Since the kind is [best], it should normalize away *)
module M : sig
  type t : immediate with extensible
end = struct
  type t
end
[%%expect{|
module M : sig type t end
|}]
