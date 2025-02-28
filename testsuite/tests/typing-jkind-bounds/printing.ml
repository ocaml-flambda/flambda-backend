(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)

(* Test printing errors *)

type t : immediate = A of int
[%%expect {|
Line 1, characters 0-29:
1 | type t : immediate = A of int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immediate
         because of the annotation on the declaration of the type t.
|}]

type 'a t : immutable_data = A of 'a
[%%expect {|
Line 1, characters 0-36:
1 | type 'a t : immutable_data = A of 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type ('a, 'b) t : immutable_data with 'a = { a : 'a; b : 'b }
[%%expect {|
Line 1, characters 0-61:
1 | type ('a, 'b) t : immutable_data with 'a = { a : 'a; b : 'b }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a with 'b
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data with 'a
         because of the annotation on the declaration of the type t.
|}]

type 'a t : immutable_data = Foo of 'a @@ portable
[%%expect {|
Line 1, characters 0-50:
1 | type 'a t : immutable_data = Foo of 'a @@ portable
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a @@ portable
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.

       The first mode-crosses less than the second along:
         linearity: mod many with 'a ≰ mod many
         contention: mod contended with 'a ≰ mod contended
         yielding: mod unyielding with 'a ≰ mod unyielding
|}]

module M : sig
  type t : immutable_data
end = struct
  type 'a t = Foo of 'a
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = Foo of 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = Foo of 'a end
       is not included in
         sig type t : immutable_data end
       Type declarations do not match:
         type 'a t = Foo of 'a
       is not included in
         type t : immutable_data
       They have different arities.
|}]

module M : sig
  type 'a t : immutable_data
end = struct
  type 'a t = Foo of 'a @@ contended many
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = Foo of 'a @@ contended many
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = Foo of 'a @@ many contended end
       is not included in
         sig type 'a t : immutable_data end
       Type declarations do not match:
         type 'a t = Foo of 'a @@ many contended
       is not included in
         type 'a t : immutable_data
       The kind of the first is immutable_data with 'a @@ many contended
         because of the definition of t at line 4, characters 2-41.
       But the kind of the first must be a subkind of immutable_data
         because of the definition of t at line 2, characters 2-28.

       The first mode-crosses less than the second along:
         portability: mod portable with 'a ≰ mod portable
         yielding: mod unyielding with 'a ≰ mod unyielding
|}]

module M : sig
  type a
  type t : immutable_data with a @@ portable
end = struct
    type a
    type t = Foo of a | Bar of a @@ contended
end
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |     type a
6 |     type t = Foo of a | Bar of a @@ contended
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a type t = Foo of a | Bar of a @@ contended end
       is not included in
         sig type a type t : immutable_data with a @@ portable end
       Type declarations do not match:
         type t = Foo of a | Bar of a @@ contended
       is not included in
         type t : immutable_data with a @@ portable
       The kind of the first is immutable_data with a
         because of the definition of t at line 6, characters 4-45.
       But the kind of the first must be a subkind of immutable_data
         with a @@ portable
         because of the definition of t at line 3, characters 2-44.

       The first mode-crosses less than the second along:
         portability: mod portable with a ≰ mod portable
|}]

type a
type ('a : immutable_data) t
let f () : a t = failwith ""
[%%expect {|
type a
type ('a : immutable_data) t
Line 3, characters 11-12:
3 | let f () : a t = failwith ""
               ^
Error: This type "a" should be an instance of type "('a : immutable_data)"
       The kind of a is value
         because of the definition of a at line 1, characters 0-6.
       But the kind of a must be a subkind of immutable_data
         because of the definition of t at line 2, characters 0-28.
|}]

type a = int ref
type ('a : immutable_data) t
let f () : a t = failwith ""
[%%expect {|
type a = int ref
type ('a : immutable_data) t
Line 3, characters 11-12:
3 | let f () : a t = failwith ""
               ^
Error: This type "a" = "int ref" should be an instance of type
         "('a : immutable_data)"
       The kind of a is mutable_data.
       But the kind of a must be a subkind of immutable_data
         because of the definition of t at line 2, characters 0-28.
|}, Principal{|
type a = int ref
type ('a : immutable_data) t
Line 3, characters 11-12:
3 | let f () : a t = failwith ""
               ^
Error: This type "a" = "int ref" should be an instance of type
         "('a : immutable_data)"
       The kind of a is mutable_data with int @@ unyielding many.
       But the kind of a must be a subkind of immutable_data
         because of the definition of t at line 2, characters 0-28.

       The first mode-crosses less than the second along:
         contention: mod uncontended ≰ mod contended
         portability: mod portable with int ≰ mod portable
|}]

type 'a u = Foo of 'a @@ portable
type ('a : immutable_data) t
let f () : (int -> int) u t = failwith ""
[%%expect {|
type 'a u = Foo of 'a @@ portable
type ('a : immutable_data) t
Line 3, characters 11-25:
3 | let f () : (int -> int) u t = failwith ""
               ^^^^^^^^^^^^^^
Error: This type "(int -> int) u" should be an instance of type
         "('a : immutable_data)"
       The kind of (int -> int) u is value mod contended portable
         because of the definition of u at line 1, characters 0-33.
       But the kind of (int -> int) u must be a subkind of immutable_data
         because of the definition of t at line 2, characters 0-28.
|}, Principal{|
type 'a u = Foo of 'a @@ portable
type ('a : immutable_data) t
Line 3, characters 11-25:
3 | let f () : (int -> int) u t = failwith ""
               ^^^^^^^^^^^^^^
Error: This type "(int -> int) u" should be an instance of type
         "('a : immutable_data)"
       The kind of (int -> int) u is immutable_data
         with int -> int @@ portable
         because of the definition of u at line 1, characters 0-33.
       But the kind of (int -> int) u must be a subkind of immutable_data
         because of the definition of t at line 2, characters 0-28.

       The first mode-crosses less than the second along:
         linearity: mod many with int -> int ≰ mod many
         contention: mod contended with int -> int ≰ mod contended
         yielding: mod unyielding with int -> int ≰ mod unyielding
|}]

module M : sig
  type t : value mod portable
end = struct
  type t : value mod contended
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : value mod contended
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : value mod contended end
       is not included in
         sig type t : value mod portable end
       Type declarations do not match:
         type t : value mod contended
       is not included in
         type t : value mod portable
       The kind of the first is value mod contended
         because of the definition of t at line 4, characters 2-30.
       But the kind of the first must be a subkind of value mod portable
         because of the definition of t at line 2, characters 2-29.
|}]

module M : sig
  type 'a t : value mod portable with 'a
end = struct
  type 'a t : value mod contended with 'a
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t : value mod contended with 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : value mod contended with 'a end
       is not included in
         sig type 'a t : value mod portable with 'a end
       Type declarations do not match:
         type 'a t : value mod contended with 'a
       is not included in
         type 'a t : value mod portable with 'a
       The kind of the first is value mod contended with 'a
         because of the definition of t at line 4, characters 2-41.
       But the kind of the first must be a subkind of value mod portable
         with 'a
         because of the definition of t at line 2, characters 2-40.
|}]

module M : sig
  type 'a t : value mod portable contended with 'a
end = struct
  type 'a t : value mod contended with 'a
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t : value mod contended with 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : value mod contended with 'a end
       is not included in
         sig type 'a t : value mod contended portable with 'a end
       Type declarations do not match:
         type 'a t : value mod contended with 'a
       is not included in
         type 'a t : value mod contended portable with 'a
       The kind of the first is value mod contended with 'a
         because of the definition of t at line 4, characters 2-41.
       But the kind of the first must be a subkind of
         value mod contended portable with 'a
         because of the definition of t at line 2, characters 2-50.
|}]

module M : sig
  type 'a t : value mod portable with 'a @@ portable
end = struct
  type 'a t : value mod portable with 'a
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t : value mod portable with 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : value mod portable with 'a end
       is not included in
         sig type 'a t : value mod portable end
       Type declarations do not match:
         type 'a t : value mod portable with 'a
       is not included in
         type 'a t : value mod portable
       The kind of the first is value mod portable with 'a
         because of the definition of t at line 4, characters 2-40.
       But the kind of the first must be a subkind of value mod portable
         because of the definition of t at line 2, characters 2-52.
|}]

module M : sig
  type 'a t : immutable_data with 'a ref
end = struct
  type 'a t : mutable_data with 'a
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t : mutable_data with 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : mutable_data with 'a end
       is not included in
         sig type 'a t : mutable_data with 'a @@ unyielding many end
       Type declarations do not match:
         type 'a t : mutable_data with 'a
       is not included in
         type 'a t : mutable_data with 'a @@ unyielding many
       The kind of the first is mutable_data with 'a
         because of the definition of t at line 4, characters 2-34.
       But the kind of the first must be a subkind of mutable_data
         with 'a @@ unyielding many
         because of the definition of t at line 2, characters 2-40.

       The first mode-crosses less than the second along:
         linearity: mod many with 'a ≰ mod many
         yielding: mod unyielding with 'a ≰ mod unyielding
|}]

module M : sig
  type 'a t : value mod portable with 'a @@ portable
end = struct
  type 'a t : value mod portable with 'a @@ portable with 'a
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t : value mod portable with 'a @@ portable with 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : value mod portable with 'a end
       is not included in
         sig type 'a t : value mod portable end
       Type declarations do not match:
         type 'a t : value mod portable with 'a
       is not included in
         type 'a t : value mod portable
       The kind of the first is value mod portable with 'a
         because of the definition of t at line 4, characters 2-60.
       But the kind of the first must be a subkind of value mod portable
         because of the definition of t at line 2, characters 2-52.
|}]

type 'a t_mutable : mutable_data with 'a
module M : sig
  type 'a t : immutable_data with 'a list with 'a option
end = struct
  type 'a t : immutable_data with 'a t_mutable
end
[%%expect {|
type 'a t_mutable : mutable_data with 'a
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type 'a t : immutable_data with 'a t_mutable
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : immutable_data with 'a t_mutable end
       is not included in
         sig type 'a t : immutable_data with 'a end
       Type declarations do not match:
         type 'a t : immutable_data with 'a t_mutable
       is not included in
         type 'a t : immutable_data with 'a
       The kind of the first is immutable_data with 'a t_mutable
         because of the definition of t at line 5, characters 2-46.
       But the kind of the first must be a subkind of immutable_data with 'a
         because of the definition of t at line 3, characters 2-56.
|}]

module M : sig
  type 'a t : immutable_data with 'a list with 'a option
end = struct
  type 'a t : immutable_data with 'a ref
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t : immutable_data with 'a ref
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : mutable_data with 'a @@ unyielding many end
       is not included in
         sig type 'a t : immutable_data with 'a end
       Type declarations do not match:
         type 'a t : mutable_data with 'a @@ unyielding many
       is not included in
         type 'a t : immutable_data with 'a
       The kind of the first is mutable_data with 'a @@ unyielding many
         because of the definition of t at line 4, characters 2-40.
       But the kind of the first must be a subkind of immutable_data with 'a
         because of the definition of t at line 2, characters 2-56.

       The first mode-crosses less than the second along:
         contention: mod uncontended ≰ mod contended with 'a
|}]
