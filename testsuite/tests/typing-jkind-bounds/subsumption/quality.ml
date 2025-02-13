(* TEST
    flags = "-extension layouts_alpha -infer-with-bounds";
    expect;
*)

(* Test that not-best kinds are respected *)

module M : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t end
       is not included in
         sig type 'a t : immutable_data with 'a end
       Type declarations do not match:
         type 'a t
       is not included in
         type 'a t : immutable_data with 'a
       The kind of the first is value
         because of the definition of t at line 4, characters 2-11.
       But the kind of the first must be a subkind of immutable_data with 'a
         because of the definition of t at line 2, characters 2-36.
|}]

(* This appears sound to accept. But it isn't. See the following test. *)
module M : sig
  type a
  type t : value mod portable with a
end = struct
  type a
  type t
end
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type a
6 |   type t
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a type t end
       is not included in
         sig type a type t : value mod portable with a end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod portable with a
       The kind of the first is value
         because of the definition of t at line 6, characters 2-8.
       But the kind of the first must be a subkind of value mod portable
         with a
         because of the definition of t at line 3, characters 2-36.
|}]

(* This test demonstrates why the above shouldn't be accepted. We can learn more about
   a via gadt refinement *)
type ('a, 'b) eq = Eq : ('a, 'a) eq

module M : sig
  type a
  type t : value mod portable with a
  val a_is_int : (a, int) eq
end = struct
  type a = int
  type t : value mod portable with a
  let a_is_int : (a, int) eq = Eq
end

let f (x : M.t @@ nonportable) : M.t @@ portable =
  match M.a_is_int with
  | Eq -> x
[%%expect {|
type ('a, 'b) eq = Eq : ('a, 'a) eq
module M :
  sig
    type a
    type t : value mod portable with a
    val a_is_int : (a, int) eq
  end
val f : M.t -> M.t @ portable = <fun>
|}]

type a
type b = a
type u
type t : value mod global with a = u
[%%expect {|
type a
type b = a
type u
Line 4, characters 0-36:
4 | type t : value mod global with a = u
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "u" is value
         because of the definition of u at line 3, characters 0-6.
       But the kind of type "u" must be a subkind of value mod global with a
         because of the definition of t at line 4, characters 0-36.
|}]

module F (M : sig type t end) = struct
  module type S = sig
    type t : value mod global with M.t
  end
  type t
  module type T = S with type t = t
end
[%%expect {|
Line 6, characters 25-35:
6 |   module type T = S with type t = t
                             ^^^^^^^^^^
Error: The kind of type "t" is value
         because of the definition of t at line 5, characters 2-8.
       But the kind of type "t" must be a subkind of value mod global with M.t
         because of the definition of t at line 3, characters 4-38.
|}]

module type S = sig
  type t
end

type a
module M : S with type t = a = struct
  type t = a
end

module _ : sig
  type t : value mod portable contended with M.t
end = struct
  type t : value mod portable
end
[%%expect {|
module type S = sig type t end
type a
module M : sig type t = a end
Lines 12-14, characters 6-3:
12 | ......struct
13 |   type t : value mod portable
14 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : value mod portable end
       is not included in
         sig type t : value mod contended portable with M.t end
       Type declarations do not match:
         type t : value mod portable
       is not included in
         type t : value mod contended portable with M.t
       The kind of the first is value mod portable
         because of the definition of t at line 13, characters 2-29.
       But the kind of the first must be a subkind of
         value mod contended portable with M.t
         because of the definition of t at line 11, characters 2-48.
|}]

module type S = sig
  type t
  type u = t
end

type a
module M : S with type t := a = struct
  type t = a
  type u = a
end

module M : sig
  type t : value mod portable contended with M.u
end = struct
  type t : value mod portable
end
[%%expect {|
module type S = sig type t type u = t end
type a
module M : sig type u = a end
Lines 14-16, characters 6-3:
14 | ......struct
15 |   type t : value mod portable
16 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : value mod portable end
       is not included in
         sig type t : value mod contended portable with M.u end
       Type declarations do not match:
         type t : value mod portable
       is not included in
         type t : value mod contended portable with M.u
       The kind of the first is value mod portable
         because of the definition of t at line 15, characters 2-29.
       But the kind of the first must be a subkind of
         value mod contended portable with M.u
         because of the definition of t at line 13, characters 2-48.
|}]

module M : sig
  type a = [`a of string | `b]
  type t : value mod global with a
end = struct
  type a = [`a of string | `b]
  type t
end
(* CR layouts v2.8: this is fine to accept *)
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type a = [`a of string | `b]
6 |   type t
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a = [ `a of string | `b ] type t end
       is not included in
         sig
           type a = [ `a of string | `b ]
           type t : value mod global with a
         end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod global with a
       The kind of the first is value
         because of the definition of t at line 6, characters 2-8.
       But the kind of the first must be a subkind of value mod global with a
         because of the definition of t at line 3, characters 2-34.
|}]

module M : sig
  type 'a u = [< `a of string | `b] as 'a
  type 'a t : value mod global with 'a u
end = struct
  type 'a u = [< `a of string | `b] as 'a
  type 'a t constraint 'a = [< `a of string | `b]
end
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type 'a u = [< `a of string | `b] as 'a
6 |   type 'a t constraint 'a = [< `a of string | `b]
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type 'a u = 'a constraint 'a = [< `a of string | `b ]
           type 'a t constraint 'a = [< `a of string | `b ]
         end
       is not included in
         sig
           type 'a u = 'a constraint 'a = [< `a of string | `b ]
           type 'a t : value mod global with [< `a of string | `b ] u
             constraint 'a = [< `a of string | `b ]
         end
       Type declarations do not match:
         type 'a t constraint 'a = [< `a of string | `b ]
       is not included in
         type 'a t : value mod global with [< `a of string | `b ] u
           constraint 'a = [< `a of string | `b ]
       The kind of the first is value
         because of the definition of t at line 6, characters 2-49.
       But the kind of the first must be a subkind of value mod global
         with [< `a of string | `b ] u
         because of the definition of t at line 3, characters 2-40.
|}]

module M : sig
  type 'a u = [> `a of string | `b] as 'a
  type 'a t : value mod portable with 'a u
end = struct
  type 'a u = [> `a of string | `b] as 'a
  type 'a t constraint 'a = [> `a of string | `b]
end
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type 'a u = [> `a of string | `b] as 'a
6 |   type 'a t constraint 'a = [> `a of string | `b]
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type 'a u = 'a constraint 'a = [> `a of string | `b ]
           type 'a t constraint 'a = [> `a of string | `b ]
         end
       is not included in
         sig
           type 'a u = 'a constraint 'a = [> `a of string | `b ]
           type 'a t : value mod portable with [> `a of string | `b ] u
             constraint 'a = [> `a of string | `b ]
         end
       Type declarations do not match:
         type 'a t constraint 'a = [> `a of string | `b ]
       is not included in
         type 'a t : value mod portable with [> `a of string | `b ] u
           constraint 'a = [> `a of string | `b ]
       The kind of the first is value
         because of the definition of t at line 6, characters 2-49.
       But the kind of the first must be a subkind of value mod portable
         with [> `a of string | `b ] u
         because of the definition of t at line 3, characters 2-42.
|}]

module M : sig
  type a = < value : string >
  type t : value mod global with a
end = struct
  type a = < value : string >
  type t
end
(* CR layouts v2.8: this is fine to accept *)
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type a = < value : string >
6 |   type t
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a = < value : string > type t end
       is not included in
         sig type a = < value : string > type t : value mod global with a end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod global with a
       The kind of the first is value
         because of the definition of t at line 6, characters 2-8.
       But the kind of the first must be a subkind of value mod global with a
         because of the definition of t at line 3, characters 2-34.
|}]

(* CR layouts v2.8: gadts shouldn't be "best" because we intend to give them more refined
   jkinds in the future. So this program will error in the future. *)
type gadt = Foo : int -> gadt
module M : sig
  type t : value mod portable with gadt
end = struct
  type t
end
[%%expect {|
type gadt = Foo : int -> gadt
module M : sig type t end
|}]

(* CR layouts v2.8: gadts shouldn't be "best". But maybe they should track quality along
   individual axes, and so this should be accepted anyways? *)
type gadt = Foo : int -> gadt
module M : sig
  type t : value mod global with gadt
end = struct
  type t
end
[%%expect {|
type gadt = Foo : int -> gadt
module M : sig type t end
|}]

type gadt = Foo : int -> gadt [@@unboxed]
module M : sig
  type t : value mod portable with gadt
end = struct
  type t
end
[%%expect {|
type gadt = Foo : int -> gadt [@@unboxed]
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type t
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t end
       is not included in
         sig type t : value mod portable end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod portable
       The kind of the first is value
         because of the definition of t at line 5, characters 2-8.
       But the kind of the first must be a subkind of value mod portable
         because of the definition of t at line 3, characters 2-39.
|}]

module M : sig
  type a = int ref * int
  type t : value mod contended with a
end = struct
  type a = int ref * int
  type t
end
(* CR layouts v2.8: this should be accepted *)
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type a = int ref * int
6 |   type t
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a = int ref * int type t end
       is not included in
         sig type a = int ref * int type t : value mod contended with a end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod contended with a
       The kind of the first is value
         because of the definition of t at line 6, characters 2-8.
       But the kind of the first must be a subkind of value mod contended
         with a
         because of the definition of t at line 3, characters 2-37.
|}]

module M : sig
  type a = #(int ref * int)
  type t : value mod contended with a
end = struct
  type a = #(int ref * int)
  type t
end
[%%expect {|
module M : sig type a = #(int ref * int) type t end
|}]

module M : sig
  type a = int -> int
  type t : value mod portable with a
end = struct
  type a = int -> int
  type t
end
[%%expect {|
module M : sig type a = int -> int type t end
|}]

module M : sig
  type a = { foo : 'a. 'a ref } [@@unboxed]
  type t : value mod contended with a
end = struct
  type a = { foo : 'a. 'a ref } [@@unboxed]
  type t
end
[%%expect {|
module M : sig type a = { foo : 'a. 'a ref; } [@@unboxed] type t end
|}]

module M : sig
  type a = { foo : ('a : value). 'a } [@@unboxed]
  type t : value mod contended with a
end = struct
  type a = { foo : ('a : value). 'a } [@@unboxed]
  type t
end
(* CR layouts v2.8: maybe this should be accepted? *)
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type a = { foo : ('a : value). 'a } [@@unboxed]
6 |   type t
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a = { foo : 'a. 'a; } [@@unboxed] type t end
       is not included in
         sig
           type a = { foo : 'a. 'a; } [@@unboxed]
           type t : value mod contended with a
         end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod contended with a
       The kind of the first is value
         because of the definition of t at line 6, characters 2-8.
       But the kind of the first must be a subkind of value mod contended
         with a
         because of the definition of t at line 3, characters 2-37.
|}]

module type S = sig
  val nonportable_f : int -> int
end
type s = (module S)
module M : sig
  type t : value mod portable with s
end = struct
  type t
end
(* CR layouts v2.8: this should be accepted because module types should be best
   (once we start giving them proper kinds) *)
[%%expect {|
module type S = sig val nonportable_f : int -> int end
type s = (module S)
Lines 7-9, characters 6-3:
7 | ......struct
8 |   type t
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t end
       is not included in
         sig type t : value mod portable with s end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod portable with s
       The kind of the first is value
         because of the definition of t at line 8, characters 2-8.
       But the kind of the first must be a subkind of value mod portable
         with s
         because of the definition of t at line 6, characters 2-36.
|}]

type a : value = private int
module M : sig
  type t : value mod portable with a
end = struct
  type t
end
[%%expect {|
type a = private int
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type t
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t end
       is not included in
         sig type t : value mod portable end
       Type declarations do not match:
         type t
       is not included in
         type t : value mod portable
       The kind of the first is value
         because of the definition of t at line 5, characters 2-8.
       But the kind of the first must be a subkind of value mod portable
         because of the definition of t at line 3, characters 2-36.
|}]

type a : value mod many = private string
module M : sig
  type t : value mod many portable with a
end = struct
  type t : value mod many
end
[%%expect {|
type a = private string
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type t : value mod many
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : value mod many end
       is not included in
         sig type t : value mod many portable end
       Type declarations do not match:
         type t : value mod many
       is not included in
         type t : value mod many portable
       The kind of the first is value mod many
         because of the definition of t at line 5, characters 2-25.
       But the kind of the first must be a subkind of value mod many portable
         because of the definition of t at line 3, characters 2-41.
|}]

type a = { foo : int -> int }
module M : sig
  type t : value mod portable with a
end = struct
  type t
end
[%%expect {|
type a = { foo : int -> int; }
module M : sig type t end
|}]

type a = Foo of (int -> int)
module M : sig
  type t : value mod portable with a
end = struct
  type t
end
[%%expect {|
type a = Foo of (int -> int)
module M : sig type t end
|}]

type a = ..
module M : sig
  type t : value mod portable with a
end = struct
  type t
end
[%%expect {|
type a = ..
module M : sig type t end
|}]
