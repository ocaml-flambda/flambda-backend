(* TEST
 {
   flags = "-allow-illegal-crossing";
   expect;
 }
*)

(*************************************************************************************)
(* Test 1: nominative types can cross portability and contention axes when annotated *)

type a
type b : value mod portable = { a : int; b : int }
[%%expect {|
type a
type b : value mod portable = { a : int; b : int; }
|}]

type a
type b : value mod uncontended = Foo of int
[%%expect {|
type a
type b : value mod uncontended = Foo of int
|}]

type a
type b : value mod uncontended portable = Foo of int | Bar of int
[%%expect {|
type a
type b : value mod uncontended portable = Foo of int | Bar of int
|}]

module _ = struct
  type a
  type b : value mod uncontended = private a
end
[%%expect {|
Line 3, characters 2-44:
3 |   type b : value mod uncontended = private a
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 2, characters 2-8.
       But the kind of type a must be a subkind of value mod uncontended
         because of the definition of b at line 3, characters 2-44.
|}]

type t : value mod portable uncontended = { a : int; b : int }
[%%expect {|
type t : value mod portable uncontended = { a : int; b : int; }
|}]

type ('a, 'b) t : value mod portable uncontended = { a : 'a; b : 'b }
[%%expect {|
type ('a, 'b) t : value mod portable uncontended = { a : 'a; b : 'b; }
|}]

type t : value mod portable = private { foo : string }
[%%expect {|
type t : value mod portable = private { foo : string; }
|}]

type a : value mod portable = { foo : string }
type b : value mod portable = a = { foo : string }
[%%expect {|
type a : value mod portable = { foo : string; }
type b = a : value mod portable = { foo : string; }
|}]

type a : value mod uncontended = private { foo : string }
type b : value mod uncontended = a = private { foo : string }
[%%expect {|
type a : value mod uncontended = private { foo : string; }
type b = a : value mod uncontended = private { foo : string; }
|}]

type t : value mod uncontended = private Foo of int | Bar
[%%expect {|
type t : value mod uncontended = private Foo of int | Bar
|}]

type a : value mod uncontended = Foo of int | Bar
type b : value mod uncontended = a = Foo of int | Bar
[%%expect {|
type a : value mod uncontended = Foo of int | Bar
type b = a : value mod uncontended = Foo of int | Bar
|}]

type a : value mod portable = private Foo of int | Bar
type b : value mod portable = a = private Foo of int | Bar
[%%expect {|
type a : value mod portable = private Foo of int | Bar
type b = a : value mod portable = private Foo of int | Bar
|}]

module A : sig
  type t : value mod portable = { a : string }
end = struct
  type t = { a : string }
end
[%%expect {|
module A : sig type t : value mod portable = { a : string; } end
|}]

(********************************************)
(* Test 2: illegal mode crossing propogates *)

type a : value mod portable uncontended = Foo of string
type ('a : value mod portable uncontended) b
type c = a b
[%%expect {|
type a : value mod portable uncontended = Foo of string
type ('a : value mod uncontended portable) b
type c = a b
|}]

type t : value mod portable uncontended = { a : string; b : int }
let f : ('a : value mod portable uncontended). 'a -> 'a = fun x -> x
let g (x : t) = f x
[%%expect {|
type t : value mod portable uncontended = { a : string; b : int; }
val f : ('a : value mod uncontended portable). 'a -> 'a = <fun>
val g : t -> t = <fun>
|}]

type t : value mod portable uncontended = { a : int; b : int }
let x : _ as (_ : value mod portable uncontended) = { a = 5; b = 5 }
[%%expect {|
type t : value mod portable uncontended = { a : int; b : int; }
val x : t = {a = 5; b = 5}
|}]

type ('a, 'b) t : value mod portable uncontended = { a : 'a; b : 'b }
let x : _ as (_ : value mod portable uncontended) = { a = 5; b = 5 }
[%%expect {|
type ('a, 'b) t : value mod portable uncontended = { a : 'a; b : 'b; }
val x : (int, int) t = {a = 5; b = 5}
|}]

type t : value mod portable uncontended = Foo of string | Bar of int
let x : _ as (_ : value mod portable uncontended) = Foo "hello world"
[%%expect {|
type t : value mod portable uncontended = Foo of string | Bar of int
val x : t = Foo "hello world"
|}]

module A : sig
  type t : value mod portable = { a : string }
end = struct
  type t = { a : string }
end
type ('a : value mod portable) u = 'a
type v = A.t u
let x : _ as (_ : value mod portable) = ({ a = "hello" } : A.t)
[%%expect {|
module A : sig type t : value mod portable = { a : string; } end
type ('a : value mod portable) u = 'a
type v = A.t u
val x : A.t = {A.a = "hello"}
|}]

type t : value mod portable = { a : string }
let my_str : string @@ nonportable = ""
let y = ({ a = my_str } : t @@ portable)
[%%expect {|
type t : value mod portable = { a : string; }
val my_str : string = ""
val y : t = {a = ""}
|}]

type t : value mod portable = { a : string -> string }
let my_fun @ nonportable = fun x -> x
let y : t @@ portable = { a = my_fun }
[%%expect {|
type t : value mod portable = { a : string -> string; }
val my_fun : 'a -> 'a = <fun>
val y : t = {a = <fun>}
|}]

type t : value mod uncontended = { a : string }
let make_str () : string = failwith ""
let f () =
  let _ = ({ a = make_str () } : t @@ uncontended) in
  ()
[%%expect {|
type t : value mod uncontended = { a : string; }
val make_str : unit -> string = <fun>
val f : unit -> unit = <fun>
|}]

type t : value mod uncontended = { a : string }
let make_str () : string = failwith ""
let f () =
  let _ : t @@ uncontended = { a = make_str () } in
  ()
[%%expect {|
type t : value mod uncontended = { a : string; }
val make_str : unit -> string = <fun>
val f : unit -> unit = <fun>
|}]
(* CR layouts v2.8: this is unfortunate that this isn't accepted, but it is fine
   since pushing the annotation to the right hand side resolves the issue, and
   -allow-illegal-crossing is a short-term solution *)

type t_value : value
type t : value mod portable uncontended = Foo of t_value
let make_value () : t_value = failwith ""
let f () =
  let _ = (Foo (make_value ()) : t @@ portable uncontended) in
  ()
[%%expect {|
type t_value : value
type t : value mod portable uncontended = Foo of t_value
val make_value : unit -> t_value = <fun>
val f : unit -> unit = <fun>
|}]

type t : value mod portable = { a : string -> string }
let my_fun : _ @@ nonportable = fun x -> x
let y = ({ a = my_fun } : _ @@ portable)
[%%expect {|
type t : value mod portable = { a : string -> string; }
val my_fun : 'a -> 'a = <fun>
Line 3, characters 15-21:
3 | let y = ({ a = my_fun } : _ @@ portable)
                   ^^^^^^
Error: This value is nonportable but expected to be portable.
|}]
(* CR layouts v2.8: this is unfortunate that this isn't accepted, but it is fine
   since adding the type to the annotation resolves the issue, and
   -allow-illegal-crossing is a short-term solution *)

let f (_x : _ @@ portable uncontended) = ()
type t : value mod portable uncontended = Foo of string | Bar of int
let g (x : t @@ nonportable contended) = f x; f (Foo ""); f (Bar 10)
[%%expect {|
val f : 'a @ portable -> unit = <fun>
type t : value mod portable uncontended = Foo of string | Bar of int
val g : t @ contended -> unit = <fun>
|}]

(* Demonstrate that -allow-illegal-crossing allows for unsound mode-crossing *)
module Unsound : sig
  val cross : 'a @ nonportable contended -> 'a @ portable uncontended
end = struct
  type 'a box : value mod portable uncontended = { value : 'a }
  let cross x =
    let box = { value = x } in
    box.value
end

module Value : sig
  type t
  val value : t
end = struct
  type t = Foo
  let value = Foo
end

let x : Value.t @@ portable uncontended = Unsound.cross Value.value
[%%expect {|
module Unsound : sig val cross : 'a @ contended -> 'a @ portable end
module Value : sig type t val value : t end
val x : Value.t = <abstr>
|}]

(* Validate above testing technique *)
let x : Value.t @@ portable uncontended = Value.value
[%%expect {|
Line 1, characters 42-53:
1 | let x : Value.t @@ portable uncontended = Value.value
                                              ^^^^^^^^^^^
Error: This value is nonportable but expected to be portable.
|}]
(********************************************************)
(* Test 3: types cannot cross other axes when annotated *)

type a
type b : value mod global = private a
[%%expect {|
type a
Line 2, characters 0-37:
2 | type b : value mod global = private a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-6.
       But the kind of type a must be a subkind of value mod global
         because of the definition of b at line 2, characters 0-37.
|}]

type a
type b : value mod many = private a
[%%expect {|
type a
Line 2, characters 0-35:
2 | type b : value mod many = private a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-6.
       But the kind of type a must be a subkind of value mod many
         because of the definition of b at line 2, characters 0-35.
|}]

type a
type b : value mod unique = private a
[%%expect {|
type a
Line 2, characters 0-37:
2 | type b : value mod unique = private a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-6.
       But the kind of type a must be a subkind of value mod unique
         because of the definition of b at line 2, characters 0-37.
|}]

type a
type b : immediate = private a
[%%expect {|
type a
Line 2, characters 0-30:
2 | type b : immediate = private a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-6.
       But the kind of type a must be a subkind of immediate
         because of the definition of b at line 2, characters 0-30.
|}]

module _ = struct
  type a
  type b : value mod global = private a
end
[%%expect {|
Line 3, characters 2-39:
3 |   type b : value mod global = private a
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a/2 is value
         because of the definition of a at line 2, characters 2-8.
       But the kind of type a/2 must be a subkind of value mod global
         because of the definition of b at line 3, characters 2-39.
|}]

module A : sig
  type t
end = struct
  type t : value mod many = private t_value
end
[%%expect {|
Line 4, characters 2-43:
4 |   type t : value mod many = private t_value
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t_value is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type t_value must be a subkind of value mod many
         because of the definition of t at line 4, characters 2-43.
|}]

type t : value mod external_ = private t_value
[%%expect {|
Line 1, characters 0-46:
1 | type t : value mod external_ = private t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t_value is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type t_value must be a subkind of value mod external_
         because of the definition of t at line 1, characters 0-46.
|}]

type t : value mod global = { a : int; b : int }
[%%expect {|
Line 1, characters 0-48:
1 | type t : value mod global = { a : int; b : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed record type.
       But the kind of type t must be a subkind of value mod global
         because of the annotation on the declaration of the type t.
|}]

type ('a, 'b) t : value mod many = { a : 'a; b : 'b }
[%%expect {|
Line 1, characters 0-53:
1 | type ('a, 'b) t : value mod many = { a : 'a; b : 'b }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t is value
         because it's a boxed record type.
       But the kind of type t must be a subkind of value mod many
         because of the annotation on the declaration of the type t.
|}]

type a : value mod unique = private b
and b
[%%expect {|
Line 1, characters 0-37:
1 | type a : value mod unique = private b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type b is value
         because an abstract type has the value kind by default.
       But the kind of type b must be a subkind of value mod unique
         because of the definition of a at line 1, characters 0-37.
|}]

type a
and b : value mod global = private a
[%%expect {|
Line 2, characters 0-36:
2 | and b : value mod global = private a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a/3 is value
         because an abstract type has the value kind by default.
       But the kind of type a/3 must be a subkind of value mod global
         because of the definition of b at line 2, characters 0-36.
|}]

module rec A : sig
  type t : value mod external_
end = struct
  type t : value mod external_ = private B.t
end
and B : sig
  type t
end = struct
  type t
end
[%%expect {|
Line 4, characters 2-44:
4 |   type t : value mod external_ = private B.t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type B.t is value
         because of the definition of t at line 7, characters 2-8.
       But the kind of type B.t must be a subkind of value mod external_
         because of the definition of t at line 4, characters 2-44.
|}]

module rec A : sig
  type t
end = struct
  type t
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
Error: The kind of type A.t is value
         because of the definition of t at line 2, characters 2-8.
       But the kind of type A.t must be a subkind of value mod many
         because of the definition of t at line 9, characters 2-39.
|}]

(*********************************************************************************)
(* Test 4: types cannot cross portability and contention axes when not annotated *)

module A : sig
  type t : value mod portable
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
         sig type t : value mod portable end
       Type declarations do not match:
         type t = { a : string; }
       is not included in
         type t : value mod portable
       The kind of the first is value
         because of the definition of t at line 4, characters 2-25.
       But the kind of the first must be a subkind of value mod portable
         because of the definition of t at line 2, characters 2-29.
|}]

module A : sig
  type t : value mod portable = { a : string }
end = struct
  type t = { a : string }
  type ('a : value mod portable) u = 'a
  type v = t u
end
[%%expect {|
Line 6, characters 11-12:
6 |   type v = t u
               ^
Error: This type t should be an instance of type ('a : value mod portable)
       The kind of t is value
         because of the definition of t at line 4, characters 2-25.
       But the kind of t must be a subkind of value mod portable
         because of the definition of u at line 5, characters 2-39.
|}]

module A : sig
  type t : value mod portable = { a : string }
end = struct
  type t = { a : string }
  let x : _ as (_ : value mod portable) = { a = "hello" }
end
[%%expect {|
Line 5, characters 42-57:
5 |   let x : _ as (_ : value mod portable) = { a = "hello" }
                                              ^^^^^^^^^^^^^^^
Error: This expression has type t but an expression was expected of type
         ('a : value mod portable)
       The kind of t is value
         because of the definition of t at line 4, characters 2-25.
       But the kind of t must be a subkind of value mod portable
         because of the annotation on the wildcard _ at line 5, characters 20-38.
|}]

type a
type b : value mod portable = a
[%%expect {|
type a
Line 2, characters 0-31:
2 | type b : value mod portable = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-6.
       But the kind of type a must be a subkind of value mod portable
         because of the definition of b at line 2, characters 0-31.
|}]

type a = { foo : int; bar : string }
type b : any mod portable = a
[%%expect {|
type a = { foo : int; bar : string; }
Line 2, characters 0-29:
2 | type b : any mod portable = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-36.
       But the kind of type a must be a subkind of any mod portable
         because of the definition of b at line 2, characters 0-29.
|}]

type a = Foo of int | Bar of string
type b : any mod uncontended = a
[%%expect {|
type a = Foo of int | Bar of string
Line 2, characters 0-32:
2 | type b : any mod uncontended = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-35.
       But the kind of type a must be a subkind of any mod uncontended
         because of the definition of b at line 2, characters 0-32.
|}]

module Foo : sig
  type t
end = struct
  type t : value mod portable = string
end
type t : value mod portable = Foo.t
[%%expect {|
module Foo : sig type t end
Line 6, characters 0-35:
6 | type t : value mod portable = Foo.t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type Foo.t is value
         because of the definition of t at line 2, characters 2-8.
       But the kind of type Foo.t must be a subkind of value mod portable
         because of the definition of t at line 6, characters 0-35.
|}]

type a = { foo : string }
type b : value mod portable = a = { foo : string }
[%%expect {|
type a = { foo : string; }
Line 2, characters 0-50:
2 | type b : value mod portable = a = { foo : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-25.
       But the kind of type a must be a subkind of value mod portable
         because of the definition of b at line 2, characters 0-50.
|}]

type a = private { foo : string }
type b : value mod uncontended = a = private { foo : string }
[%%expect {|
type a = private { foo : string; }
Line 2, characters 0-61:
2 | type b : value mod uncontended = a = private { foo : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-33.
       But the kind of type a must be a subkind of value mod uncontended
         because of the definition of b at line 2, characters 0-61.
|}]

type a = Foo of string | Bar
type b : value mod uncontended = a = Foo of string | Bar
[%%expect {|
type a = Foo of string | Bar
Line 2, characters 0-56:
2 | type b : value mod uncontended = a = Foo of string | Bar
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-28.
       But the kind of type a must be a subkind of value mod uncontended
         because of the definition of b at line 2, characters 0-56.
|}]

type a = private Foo of string | Bar
type b : value mod portable = a = private Foo of string | Bar
[%%expect {|
type a = private Foo of string | Bar
Line 2, characters 0-61:
2 | type b : value mod portable = a = private Foo of string | Bar
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-36.
       But the kind of type a must be a subkind of value mod portable
         because of the definition of b at line 2, characters 0-61.
|}]

type ('a : value mod uncontended) of_uncontended
type t = t_value of_uncontended
[%%expect {|
type ('a : value mod uncontended) of_uncontended
Line 2, characters 9-16:
2 | type t = t_value of_uncontended
             ^^^^^^^
Error: This type t_value should be an instance of type
         ('a : value mod uncontended)
       The kind of t_value is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of t_value must be a subkind of value mod uncontended
         because of the definition of of_uncontended at line 1, characters 0-48.
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
       The kind of t is value
         because of the definition of t at line 2, characters 0-22.
       But the kind of t must be a subkind of value mod portable
         because of the definition of of_portable at line 1, characters 0-42.
|}]

let f : ('a : value mod portable). 'a -> 'a = fun x -> x
let _ = f (fun x -> x)
[%%expect {|
val f : ('a : value mod portable). 'a -> 'a = <fun>
Line 2, characters 10-22:
2 | let _ = f (fun x -> x)
              ^^^^^^^^^^^^
Error:
       The kind of 'a -> 'b is value mod unique uncontended
         because it's a function type.
       But the kind of 'a -> 'b must be a subkind of value mod portable
         because of the definition of f at line 1, characters 4-5.
|}]

let f : ('a : value mod uncontended). 'a -> 'a = fun x -> x
let _ = f (ref 10)
[%%expect {|
val f : ('a : value mod uncontended). 'a -> 'a = <fun>
Line 2, characters 10-18:
2 | let _ = f (ref 10)
              ^^^^^^^^
Error: This expression has type int ref
       but an expression was expected of type ('a : value mod uncontended)
       The kind of int ref is value
         because of kind requirements from an imported definition.
       But the kind of int ref must be a subkind of value mod uncontended
         because of the definition of f at line 1, characters 4-5.
|}]

(* immediate types can still cross *)
let f : ('a : value mod portable uncontended). 'a -> 'a = fun x -> x
let _ = f 0
[%%expect {|
val f : ('a : value mod uncontended portable). 'a -> 'a = <fun>
- : int = 0
|}]

(*****************************************)
(* Test 5: values cannot illegally cross *)

(* Used for below testing *)
module Value : sig
  type t
  val mk : t
end = struct
  type t = unit
  let mk = ()
end
[%%expect {|
module Value : sig type t val mk : t end
|}]

let x : _ as (_ : value mod portable) = Value.mk
[%%expect {|
Line 1, characters 40-48:
1 | let x : _ as (_ : value mod portable) = Value.mk
                                            ^^^^^^^^
Error: This expression has type Value.t
       but an expression was expected of type ('a : value mod portable)
       The kind of Value.t is value
         because of the definition of t at line 2, characters 2-8.
       But the kind of Value.t must be a subkind of value mod portable
         because of the annotation on the wildcard _ at line 1, characters 18-36.
|}]

type t = { v : Value.t }
let f _ : _ as (_ : value mod uncontended) = { v = Value.mk }
[%%expect {|
type t = { v : Value.t; }
Line 2, characters 45-61:
2 | let f _ : _ as (_ : value mod uncontended) = { v = Value.mk }
                                                 ^^^^^^^^^^^^^^^^
Error: This expression has type t but an expression was expected of type
         ('a : value mod uncontended)
       The kind of t is value
         because of the definition of t at line 1, characters 0-24.
       But the kind of t must be a subkind of value mod uncontended
         because of the annotation on the wildcard _ at line 2, characters 20-41.
|}]

type t = Foo of Value.t
let f : ('a : value mod portable). 'a -> 'a = fun _ -> Foo Value.mk
[%%expect {|
type t = Foo of Value.t
Line 2, characters 55-67:
2 | let f : ('a : value mod portable). 'a -> 'a = fun _ -> Foo Value.mk
                                                           ^^^^^^^^^^^^
Error: This expression has type t but an expression was expected of type
         ('a : value mod portable)
       The kind of t is value
         because of the definition of t at line 1, characters 0-23.
       But the kind of t must be a subkind of value mod portable
         because of the annotation on the universal variable 'a.
|}]

type t = Value.t
let x : ('a : value mod uncontended) = (Value.mk : t)
[%%expect {|
type t = Value.t
Line 2, characters 39-53:
2 | let x : ('a : value mod uncontended) = (Value.mk : t)
                                           ^^^^^^^^^^^^^^
Error: This expression has type t = Value.t
       but an expression was expected of type ('a : value mod uncontended)
       The kind of t is value
         because of the definition of t at line 2, characters 2-8.
       But the kind of t must be a subkind of value mod uncontended
         because of the annotation on the type variable 'a.
|}]

(***************************************)
(* Test 6: layout check is not ignored *)

type a : word
type b : value mod portable = a
[%%expect {|
type a : word
Line 2, characters 0-31:
2 | type b : value mod portable = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is word
         because of the definition of a at line 1, characters 0-13.
       But the layout of type a must be a sublayout of value
         because of the definition of b at line 2, characters 0-31.
|}]

type a : bits64
type b : float32 mod uncontended = a
[%%expect {|
type a : bits64
Line 2, characters 0-36:
2 | type b : float32 mod uncontended = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is bits64
         because of the definition of a at line 1, characters 0-15.
       But the layout of type a must be a sublayout of float32
         because of the definition of b at line 2, characters 0-36.
|}]

type a : any
type b : value mod uncontended = a
[%%expect {|
type a : any
Line 2, characters 0-34:
2 | type b : value mod uncontended = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type a is any
         because of the definition of a at line 1, characters 0-12.
       But the layout of type a must be a sublayout of value
         because of the definition of b at line 2, characters 0-34.
|}]

(****************************************************************)
(* Test 7: Non-nominative types cannot perform illegal crossing *)

type a
type b : value mod portable = a
[%%expect {|
type a
Line 2, characters 0-31:
2 | type b : value mod portable = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-6.
       But the kind of type a must be a subkind of value mod portable
         because of the definition of b at line 2, characters 0-31.
|}]

type a
type b : value mod uncontended = a
[%%expect {|
type a
Line 2, characters 0-34:
2 | type b : value mod uncontended = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-6.
       But the kind of type a must be a subkind of value mod uncontended
         because of the definition of b at line 2, characters 0-34.
|}]

type a
type b : value mod portable = private a
[%%expect {|
type a
Line 2, characters 0-39:
2 | type b : value mod portable = private a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-6.
       But the kind of type a must be a subkind of value mod portable
         because of the definition of b at line 2, characters 0-39.
|}]

type a
type b : value mod uncontended = private a
[%%expect {|
type a
Line 2, characters 0-42:
2 | type b : value mod uncontended = private a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value
         because of the definition of a at line 1, characters 0-6.
       But the kind of type a must be a subkind of value mod uncontended
         because of the definition of b at line 2, characters 0-42.
|}]

type a : word
type b : any mod uncontended portable = private a
[%%expect {|
type a : word
Line 2, characters 0-49:
2 | type b : any mod uncontended portable = private a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is word
         because of the definition of a at line 1, characters 0-13.
       But the kind of type a must be a subkind of
         any mod uncontended portable
         because of the definition of b at line 2, characters 0-49.
|}]

type a : value mod global many unique external_
type b : immediate = private a
[%%expect {|
type a : value mod global many unique external_
Line 2, characters 0-30:
2 | type b : immediate = private a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a is value mod global unique many external_
         because of the definition of a at line 1, characters 0-47.
       But the kind of type a must be a subkind of immediate
         because of the definition of b at line 2, characters 0-30.
|}]

module A : sig
  type t
end = struct
  type t : value mod portable = private t_value
end
[%%expect {|
Line 4, characters 2-47:
4 |   type t : value mod portable = private t_value
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type t_value is value
         because of the definition of t_value at line 1, characters 0-20.
       But the kind of type t_value must be a subkind of value mod portable
         because of the definition of t at line 4, characters 2-47.
|}]

type a : value mod portable uncontended = private b
and b
[%%expect {|
Line 1, characters 0-51:
1 | type a : value mod portable uncontended = private b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type b is value
         because an abstract type has the value kind by default.
       But the kind of type b must be a subkind of
         value mod uncontended portable
         because of the definition of a at line 1, characters 0-51.
|}]

type a
and b : value mod portable uncontended = a
[%%expect {|
Line 2, characters 0-42:
2 | and b : value mod portable uncontended = a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type a/2 is value
         because an abstract type has the value kind by default.
       But the kind of type a/2 must be a subkind of
         value mod uncontended portable
         because of the definition of b at line 2, characters 0-42.
|}]

module rec A : sig
  type t : value mod portable uncontended
end = struct
  type t : value mod portable uncontended = B.t
end
and B : sig
  type t
end = struct
  type t
end
[%%expect {|
Line 4, characters 2-47:
4 |   type t : value mod portable uncontended = B.t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type B.t is value
         because of the definition of t at line 7, characters 2-8.
       But the kind of type B.t must be a subkind of
         value mod uncontended portable
         because of the definition of t at line 4, characters 2-47.
|}]

module rec A : sig
  type t
end = struct
  type t
end
and B : sig
  type t : value mod portable uncontended
end = struct
  type t : value mod portable uncontended = private A.t
end
[%%expect {|
Line 9, characters 2-55:
9 |   type t : value mod portable uncontended = private A.t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type A.t is value
         because of the definition of t at line 2, characters 2-8.
       But the kind of type A.t must be a subkind of
         value mod uncontended portable
         because of the definition of t at line 9, characters 2-55.
|}]
