(* TEST
 expect;
 flags = "-allow-illegal-crossing";
*)

(******************************************************************************)
(* Test 1: types can cross the portability and contention axes when annotated *)

type a : value mod nonportable
type b : value mod portable = a
[%%expect {|
type a : value mod nonportable
type b : value mod portable = a
|}]

type a : value mod contended
type b : value mod uncontended = a
[%%expect {|
type a : value mod nonportable
type b : value mod portable = a
|}]

type a : value mod contended nonportable
type b : value mod uncontended portable = a
[%%expect {|
type a : value mod contended nonportable
type b : value mod uncontended portable = a
|}]

type a : word
type b : word mod uncontended portable = a
[%%expect {|
type a : word
type b : word mod uncontended portable = a
|}]

type a : value mod global many unique external_
type b : immediate = a
[%%expect {|
type a : value mod global many unique external_
type b : immediate = a
|}]

module _ = struct
  type a : value mod contended
  type b : value mod uncontended = a
end
[%%expect {|
module _ = struct
  type a : value mod contended
  type b : value mod uncontended = a
end
|}]

module A : sig
  type t
end = struct
  type t : value mod portable = int
end

type t : value mod portable = A.t
[%%expect {|
module A : sig
  type t
end = struct
  type t : value mod portable = int
end
type t : value mod portable = A.t
|}]

type 'a t : value mod portable = 'a
[%%expect {|
type 'a t : value mod portable = 'a
|}]

type t : value mod portable = string
[%%expect {|
type t : value mod portable = string
|}]

type t : value mod portable uncontended = { a : int; b : int }
[%%expect {|
type t : value mod portable uncontended = { a : int; b : int }
|}]

type ('a, 'b) t : value mod portable uncontended = { a : 'a; b : 'b }
[%%expect {|
type ('a, 'b) t : value mod portable uncontended = { a : 'a; b : 'b }
|}]

type a : value mod portable uncontended = b
and b : value
[%%expect {|
type a : value mod portable uncontended = b
and b : value
|}]

type a : value
and b : value mod portable uncontended = a
[%%expect {|
type a : value
and b : value mod portable uncontended = a
|}]

module rec A : sig
  type t : value mod portable uncontended = B.t
end = struct
  type t : value mod portable uncontended = B.t
end
and B : sig
  type t : value
end = struct
  type t : value
end
[%%expect {|
module rec A : sig
  type t : value mod portable uncontended = B.t
end = struct
  type t : value mod portable uncontended = B.t
end
and B : sig
  type t : value
end = struct
  type t : value
end
|}]

module rec A : sig
  type t : value
end = struct
  type t : value
end
and B : sig
  type t : value mod portable uncontended
end = struct
  type t : value mod portable uncontended = A.t
end
[%%expect {|
module rec A : sig
  type t : value
end = struct
  type t : value
end
and B : sig
  type t : value mod portable uncontended
end = struct
  type t : value mod portable uncontended = A.t
end
|}]

(********************************************)
(* Test 2: illegal mode crossing propogates *)

type a : value mod portable uncontended = string
type ('a : value mod portable uncontended) b
type c = a b
[%%expect {|
type a : value mod portable uncontended = string
type ('a : value mod portable uncontended) b
type c = a b
|}]

type t : value mod portable uncontended = string
let f : ('a : value mod portable uncontended). 'a -> 'a = fun x -> x
let g (x : t) = f x
[%%expect {|
type t : value mod portable uncontended = string
let f : ('a : value mod portable uncontended). 'a -> 'a = fun x -> x
let g (x : t) = f x
|}]

type t : value mod portable uncontended = { a : int; b : int }
let x : _ as (_ : value mod portable uncontended) = { a = 5; b = 5 }
[%%expect {|
type t : value mod portable uncontended = { a : int; b : int }
let x : _ as (_ : value mod portable uncontended) = { a = 5; b = 5 }
|}]

(********************************************************)
(* Test 2: types cannot cross other axes when annotated *)

(*********************************************************************************)
(* Test 3: types cannot cross portability and contention axes when not annotated *)

(*********************************************************************************)
(* Test 4: layout check is not ignored *)
