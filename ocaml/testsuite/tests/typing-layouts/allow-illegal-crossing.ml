(* TEST
 {
   flags = "-allow-illegal-crossing";
   expect;
 }
*)

(******************************************************************************)
(* Test 1: types can cross the portability and contention axes when annotated *)

type a : value mod nonportable
type b : value mod portable = private a
[%%expect {|
type a : value mod nonportable
type b = private a
|}]

type a : value mod contended
type b : value mod uncontended = private a
[%%expect {|
type a : value mod contended
type b = private a
|}]

type a : value mod contended nonportable
type b : value mod uncontended portable = private a
[%%expect {|
type a : value mod contended nonportable
type b = private a
|}]

type a : word
type b : word mod uncontended portable = private a
[%%expect {|
type a : word
type b = private a
|}]

type a : word
type b : any mod uncontended portable = private a
[%%expect {|
type a : word
type b = private a
|}]

type a : value mod global many unique external_
type b : immediate = private a
[%%expect {|
type a : value mod global many unique external_
type b = private a
|}]

module _ = struct
  type a : value mod contended
  type b : value mod uncontended = private a
end
[%%expect {|
|}]

module A : sig
  type t
end = struct
  type t : value mod portable = private string
end

type t : value mod portable = private A.t
[%%expect {|
module A : sig type t end
type t = private A.t
|}]

type 'a t : value mod portable = private 'a
[%%expect {|
type 'a t = private 'a
|}]

type t : value mod portable = private string
[%%expect {|
type t = private string
|}]

type t : value mod portable uncontended = { a : int; b : int }
[%%expect {|
type t = { a : int; b : int; }
|}]

type ('a, 'b) t : value mod portable uncontended = { a : 'a; b : 'b }
[%%expect {|
type ('a, 'b) t = { a : 'a; b : 'b; }
|}]

type a : value mod portable uncontended = private b
and b : value
[%%expect {|
type a = private b
and b : value
|}]

type a : value
and b : value mod portable uncontended = private a
[%%expect {|
type a : value
and b = private a
|}]

module rec A : sig
  type t : value mod portable uncontended
end = struct
  type t : value mod portable uncontended = private B.t
end
and B : sig
  type t : value
end = struct
  type t : value
end
[%%expect {|
module rec A : sig type t : value mod portable uncontended end
and B : sig type t : value end
|}]

module rec A : sig
  type t : value
end = struct
  type t : value
end
and B : sig
  type t : value mod portable uncontended
end = struct
  type t : value mod portable uncontended = private A.t
end
[%%expect {|
module rec A : sig type t : value end
and B : sig type t : value mod portable uncontended end
|}]

(********************************************)
(* Test 2: illegal mode crossing propogates *)

type a : value mod portable uncontended = private string
type ('a : value mod portable uncontended) b
type c = a b
[%%expect {|
type a = private string
type ('a : value mod uncontended portable) b
type c = a b
|}]

type t : value mod portable uncontended = private string
let f : ('a : value mod portable uncontended). 'a -> 'a = fun x -> x
let g (x : t) = f x
[%%expect {|
type t = private string
val f : ('a : value mod uncontended portable). 'a -> 'a = <fun>
val g : t -> t = <fun>
|}]

type t : value mod portable uncontended = { a : int; b : int }
let x : _ as (_ : value mod portable uncontended) = { a = 5; b = 5 }
[%%expect {|
type t = { a : int; b : int; }
val x : t = {a = 5; b = 5}
|}]

type ('a, 'b) t : value mod portable uncontended = { a : 'a; b : 'b }
let x : _ as (_ : value mod portable uncontended) = { a = 5; b = 5 }
[%%expect {|
type ('a, 'b) t = { a : 'a; b : 'b; }
val x : (int, int) t = {a = 5; b = 5}
|}]

type t : value mod portable uncontended = Foo of string | Bar of int
let x : _ as (_ : value mod portable uncontended) = Foo "hello world"
[%%expect {|
type t = Foo of string | Bar of int
val x : t = Foo "hello world"
|}]

(********************************************************)
(* Test 3: types cannot cross other axes when annotated *)

type a : value mod local
type b : value mod global = private a
[%%expect {|
type a : value mod local
Line 2, characters 0-37:
2 | type b : value mod global = private a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is value, because
         of the definition of a at line 1, characters 0-24.
       But the layout of type a must be a sublayout of value, because
         of the definition of b at line 2, characters 0-37.
|}]

type a : value mod once
type b : value mod many = private a
[%%expect {|
type a : value mod once
Line 2, characters 0-35:
2 | type b : value mod many = private a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is value, because
         of the definition of a at line 1, characters 0-23.
       But the layout of type a must be a sublayout of value, because
         of the definition of b at line 2, characters 0-35.
|}]

type a : value mod shared
type b : value mod unique = private a
[%%expect {|
type a : value mod shared
Line 2, characters 0-37:
2 | type b : value mod unique = private a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is value, because
         of the definition of a at line 1, characters 0-25.
       But the layout of type a must be a sublayout of value, because
         of the definition of b at line 2, characters 0-37.
|}]

type a : value
type b : immediate = private a
[%%expect {|
type a : value
Line 2, characters 0-30:
2 | type b : immediate = private a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is value, because
         of the definition of a at line 1, characters 0-14.
       But the layout of type a must be a sublayout of immediate, because
         of the definition of b at line 2, characters 0-30.
|}]

module _ = struct
  type a : value mod local
  type b : value mod global = private a
end
[%%expect {|
Line 3, characters 2-39:
3 |   type b : value mod global = private a
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a/2 is value, because
         of the definition of a at line 2, characters 2-26.
       But the layout of type a/2 must be a sublayout of value, because
         of the definition of b at line 3, characters 2-39.
|}]

module A : sig
  type t
end = struct
  type t : value mod many = private string
end
[%%expect {|
Line 4, characters 2-42:
4 |   type t : value mod many = private string
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type string is value, because
         it is the primitive value type string.
       But the layout of type string must be a sublayout of value, because
         of the definition of t at line 4, characters 2-42.
|}]

type 'a t : value mod unique = private 'a
[%%expect {|
Line 1, characters 0-41:
1 | type 'a t : value mod unique = private 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type 'a is value, because
         of the definition of t at line 1, characters 0-41.
       But the layout of type 'a must be a sublayout of value, because
         of the definition of t at line 1, characters 0-41.
|}]

type t : value mod external_ = private string
[%%expect {|
Line 1, characters 0-45:
1 | type t : value mod external_ = private string
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type string is value, because
         it is the primitive value type string.
       But the layout of type string must be a sublayout of immediate, because
         of the definition of t at line 1, characters 0-45.
|}]

type t : value mod global = { a : int; b : int }
[%%expect {|
Line 1, characters 0-48:
1 | type t : value mod global = { a : int; b : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of value, because
         of the annotation on the declaration of the type t.
|}]

type ('a, 'b) t : value mod many = { a : 'a; b : 'b }
[%%expect {|
Line 1, characters 0-53:
1 | type ('a, 'b) t : value mod many = { a : 'a; b : 'b }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of value, because
         of the annotation on the declaration of the type t.
|}]

type a : value mod unique = private b
and b : value
[%%expect {|
Line 1, characters 0-37:
1 | type a : value mod unique = private b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type b is value, because
         of the annotation on the declaration of the type b.
       But the layout of type b must be a sublayout of value, because
         of the definition of a at line 1, characters 0-37.
|}]

type a : value
and b : value mod global = private a
[%%expect {|
Line 2, characters 0-36:
2 | and b : value mod global = private a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a/3 is value, because
         of the annotation on the declaration of the type a.
       But the layout of type a/3 must be a sublayout of value, because
         of the definition of b at line 2, characters 0-36.
|}]

module rec A : sig
  type t : value mod external_
end = struct
  type t : value mod external_ = private B.t
end
and B : sig
  type t : value
end = struct
  type t : value
end
[%%expect {|
Line 4, characters 2-44:
4 |   type t : value mod external_ = private B.t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type B.t is value, because
         of the definition of t at line 7, characters 2-16.
       But the layout of type B.t must be a sublayout of immediate, because
         of the definition of t at line 4, characters 2-44.
|}]

module rec A : sig
  type t : value
end = struct
  type t : value
end
and B : sig
  type t : value mod many
end = struct
  type t : value mod many = private A.t
end
[%%expect {|
Line 9, characters 2-39:
9 |   type t : value mod many = private A.t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type A.t is value, because
         of the definition of t at line 2, characters 2-16.
       But the layout of type A.t must be a sublayout of value, because
         of the definition of t at line 9, characters 2-39.
|}]

(*********************************************************************************)
(* Test 4: types cannot cross portability and contention axes when not annotated *)

(* CR: should this be allowed? I think no, the user should have to specify at the
   definition of t *)
module A : sig
  type t : value mod contended portable
end = struct
  type t = { a : string }
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { a : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { a : string; } end
       is not included in
         sig type t : value mod contended portable end
       Type declarations do not match:
         type t = { a : string; }
       is not included in
         type t : value mod contended portable
       The layout of the first is value, because
         of the definition of t at line 4, characters 2-25.
       But the layout of the first must be a sublayout of value, because
         of the definition of t at line 2, characters 2-39.
|}]

type a
type b : value mod portable = a
[%%expect {|
type a
Line 2, characters 0-31:
2 | type b : value mod portable = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is value, because
         of the definition of a at line 1, characters 0-6.
       But the layout of type a must be a sublayout of value, because
         of the definition of b at line 2, characters 0-31.
|}]

type a = { foo : int; bar : string }
type b : any mod portable = a
[%%expect {|
type a = { foo : int; bar : string; }
Line 2, characters 0-29:
2 | type b : any mod portable = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is value, because
         of the definition of a at line 1, characters 0-36.
       But the layout of type a must be a sublayout of any, because
         of the definition of b at line 2, characters 0-29.
|}]

type a = Foo of int | Bar of string
type b : any mod uncontended = a
[%%expect {|
type a = Foo of int | Bar of string
Line 2, characters 0-32:
2 | type b : any mod uncontended = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is value, because
         of the definition of a at line 1, characters 0-35.
       But the layout of type a must be a sublayout of any, because
         of the definition of b at line 2, characters 0-32.
|}]

module Foo : sig
  type t
end = struct
  type t : value mod portable = string
end
type t : value mod portable = Foo.t
[%%expect {|
Line 4, characters 2-38:
4 |   type t : value mod portable = string
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type string is value, because
         it is the primitive value type string.
       But the layout of type string must be a sublayout of value, because
         of the definition of t at line 4, characters 2-38.
|}]

type ('a : value mod uncontended) of_uncontended
type t = string of_uncontended
[%%expect {|
type ('a : value mod uncontended) of_uncontended
Line 2, characters 9-15:
2 | type t = string of_uncontended
             ^^^^^^
Error: This type string should be an instance of type
         ('a : value mod uncontended)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of value, because
         of the definition of of_uncontended at line 1, characters 0-48.
|}]

type ('a : value mod portable) of_portable
type t = { foo : int }
type u = t of_portable
[%%expect {|
type ('a : value mod portable) of_portable
type t = { foo : int; }
Line 3, characters 9-10:
3 | type u = t of_portable
             ^
Error: This type t should be an instance of type ('a : value mod portable)
       The layout of t is value, because
         of the definition of t at line 2, characters 0-22.
       But the layout of t must be a sublayout of value, because
         of the definition of of_portable at line 1, characters 0-42.
|}]

let f : ('a : value mod portable). 'a -> 'a = fun x -> x
let _ = f "hello"
[%%expect {|
val f : ('a : value mod portable). 'a -> 'a = <fun>
Line 2, characters 10-17:
2 | let _ = f "hello"
              ^^^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : value mod portable)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of value, because
         of the definition of f at line 1, characters 4-5.
|}]

let f : ('a : value mod uncontended). 'a -> 'a = fun x -> x
let _ = f "hello"
[%%expect {|
val f : ('a : value mod uncontended). 'a -> 'a = <fun>
Line 2, characters 10-17:
2 | let _ = f "hello"
              ^^^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : value mod uncontended)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of value, because
         of the definition of f at line 1, characters 4-5.
|}]

(* immediate types can still cross *)
let f : ('a : value mod portable uncontended). 'a -> 'a = fun x -> x
let _ = f 0
[%%expect {|
val f : ('a : value mod uncontended portable). 'a -> 'a = <fun>
- : int = 0
|}]

(*********************************************************************************)
(* Test 5: layout check is not ignored *)

type a : word
type b : value mod portable = a
[%%expect {|
type a : word
Line 2, characters 0-31:
2 | type b : value mod portable = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is word, because
         of the definition of a at line 1, characters 0-13.
       But the layout of type a must be a sublayout of value, because
         of the definition of b at line 2, characters 0-31.
|}]

type a : bits64
type b : float32 mod uncontended = b
[%%expect {|
type a : bits64
Line 2, characters 0-36:
2 | type b : float32 mod uncontended = b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation b is cyclic:
         b = b
|}]

type a : any
type b : value mod uncontended = b
[%%expect {|
type a : any
Line 2, characters 0-34:
2 | type b : value mod uncontended = b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation b is cyclic:
         b = b
|}]
