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

(*********************************************************************************)
(* Test 4: types cannot cross portability and contention axes when not annotated *)

(* CR: should this be allowed? *)
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

(*********************************************************************************)
(* Test 5: layout check is not ignored *)
