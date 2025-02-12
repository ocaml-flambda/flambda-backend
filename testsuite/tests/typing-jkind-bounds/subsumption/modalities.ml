(* TEST
    flags = "-extension layouts_alpha -infer-with-bounds";
    expect;
*)

module M : sig
  type ('a, 'b) t : immutable_data with 'a
end = struct
  type ('a, 'b) t : immutable_data with 'a @@ portable
end
[%%expect {|
module M : sig type ('a, 'b) t : immutable_data with 'a end
|}]

module M : sig
  type ('a, 'b) t : immutable_data with 'a @@ portable
end = struct
  type ('a, 'b) t : immutable_data with 'a
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t : immutable_data with 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t : immutable_data with 'a end
       is not included in
         sig type ('a, 'b) t : immutable_data with 'a @@ portable end
       Type declarations do not match:
         type ('a, 'b) t : immutable_data with 'a
       is not included in
         type ('a, 'b) t : immutable_data with 'a @@ portable
       The kind of the first is immutable_data with 'a
         because of the definition of t at line 4, characters 2-42.
       But the kind of the first must be a subkind of immutable_data
         with 'a @@ portable
         because of the definition of t at line 2, characters 2-54.

       The first mode-crosses less than the second along:
         portability: mod portable with 'a ≰ mod portable
|}]

module M : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t : immutable_data with 'a @@ portable
end
[%%expect {|
module M : sig type 'a t : immutable_data with 'a end
|}]

module M : sig
  type 'a t : value mod portable
end = struct
  type 'a t : immutable_data with 'a @@ portable
end
[%%expect {|
module M : sig type 'a t : value mod portable end
|}]

module M : sig
  type 'a t : mutable_data with 'a @@ portable
end = struct
  type 'a t : mutable_data with 'a @@ portable
end
[%%expect {|
module M : sig type 'a t : mutable_data with 'a @@ portable end
|}]

type 'a u : immutable_data with 'a @@ contended
type 'a t : value mod portable = 'a u
[%%expect {|
type 'a u : immutable_data with 'a @@ contended
Line 2, characters 0-37:
2 | type 'a t : value mod portable = 'a u
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a u" is immutable_data with 'a @@ contended
         because of the definition of u at line 1, characters 0-47.
       But the kind of type "'a u" must be a subkind of value mod portable
         because of the definition of t at line 2, characters 0-37.

       The first mode-crosses less than the second along:
         portability: mod portable with 'a ≰ mod portable
|}]

module M : sig
  type 'a t : value mod global
end = struct
  type 'a t : immediate with 'a @@ global
end
[%%expect {|
module M : sig type 'a t : value mod global end
|}]

module M : sig
  type 'a t : value mod uncontended
end = struct
  type 'a t : immutable_data with 'a @@ contended
end
[%%expect {|
module M : sig type 'a t : value mod uncontended end
|}]

type 'a u : immutable_data with 'a @@ many
type 'a t : value mod many = 'a u
[%%expect {|
type 'a u : immutable_data with 'a @@ many
type 'a t = 'a u
|}]

module M : sig
  type 'a t : value mod unique
end = struct
  type 'a t : immediate with 'a @@ aliased
end
[%%expect {|
module M : sig type 'a t : value mod unique end
|}]

module M : sig
  type 'a t : value mod global unique many portable uncontended
end = struct
  type 'a t : immediate with 'a @@ aliased many contended global portable
end
[%%expect {|
module M :
  sig type 'a t : value mod global unique many uncontended portable end
|}]

module M : sig
  type ('a, 'b) t : value mod portable
end = struct
  type ('a, 'b) t : value mod portable with 'a @@ portable with 'b @@ portable
end
[%%expect {|
module M : sig type ('a, 'b) t : value mod portable end
|}]

type ('a, 'b) t : value mod portable with 'a @@ portable with 'b @@ contended

module type S = sig
  type ('a, 'b) t : value mod portable
end

module type T = S with type ('a, 'b) t = ('a, 'b) t
[%%expect {|
type ('a, 'b) t : value mod portable with 'b
module type S = sig type ('a, 'b) t : value mod portable end
Line 7, characters 23-51:
7 | module type T = S with type ('a, 'b) t = ('a, 'b) t
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "('a, 'b) t" is value mod portable with 'b
         because of the definition of t at line 1, characters 0-77.
       But the kind of type "('a, 'b) t" must be a subkind of
         value mod portable
         because of the definition of t at line 4, characters 2-38.
|}]

module M : sig
  type ('a, 'b) t : value mod portable with 'b
end = struct
  type ('a, 'b) t : value mod portable with 'a @@ portable with 'b @@ contended
end
[%%expect {|
module M : sig type ('a, 'b) t : value mod portable with 'b end
|}]

module M : sig
  type ('a, 'b) t : value mod portable with 'a @@ portable with 'b @@ contended
end = struct
  type ('a, 'b) t : value mod portable with 'b
end
[%%expect {|
module M : sig type ('a, 'b) t : value mod portable with 'b end
|}]

module M : sig
  type 'a t : immutable_data with 'a @@ portable
end = struct
  type 'a t : immutable_data with 'a @@ portable with 'a
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t : immutable_data with 'a @@ portable with 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : immutable_data with 'a end
       is not included in
         sig type 'a t : immutable_data with 'a @@ portable end
       Type declarations do not match:
         type 'a t : immutable_data with 'a
       is not included in
         type 'a t : immutable_data with 'a @@ portable
       The kind of the first is immutable_data with 'a
         because of the definition of t at line 4, characters 2-56.
       But the kind of the first must be a subkind of immutable_data
         with 'a @@ portable
         because of the definition of t at line 2, characters 2-48.

       The first mode-crosses less than the second along:
         portability: mod portable with 'a ≰ mod portable
|}]

module M : sig
  type 'a t : immutable_data with 'a @@ portable
end = struct
  type 'a t : immutable_data with 'a with 'a @@ portable
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t : immutable_data with 'a with 'a @@ portable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : immutable_data with 'a end
       is not included in
         sig type 'a t : immutable_data with 'a @@ portable end
       Type declarations do not match:
         type 'a t : immutable_data with 'a
       is not included in
         type 'a t : immutable_data with 'a @@ portable
       The kind of the first is immutable_data with 'a
         because of the definition of t at line 4, characters 2-56.
       But the kind of the first must be a subkind of immutable_data
         with 'a @@ portable
         because of the definition of t at line 2, characters 2-48.

       The first mode-crosses less than the second along:
         portability: mod portable with 'a ≰ mod portable
|}]

module M : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t : immutable_data with 'a @@ portable with 'a
end
[%%expect {|
module M : sig type 'a t : immutable_data with 'a end
|}]

type 'a u : value mod uncontended with 'a @@ global
type 'a t : value mod global = 'a u
[%%expect {|
type 'a u : value mod uncontended with 'a
Line 2, characters 0-35:
2 | type 'a t : value mod global = 'a u
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a u" is value mod uncontended with 'a
         because of the definition of u at line 1, characters 0-51.
       But the kind of type "'a u" must be a subkind of value mod global
         because of the definition of t at line 2, characters 0-35.
|}]

module M : sig
  type 'a t : immutable_data with 'a @@ contended portable
end = struct
  type 'a t : immutable_data with 'a @@ contended with 'a @@ portable
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t : immutable_data with 'a @@ contended with 'a @@ portable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : immutable_data with 'a end
       is not included in
         sig type 'a t : immutable_data with 'a @@ portable contended end
       Type declarations do not match:
         type 'a t : immutable_data with 'a
       is not included in
         type 'a t : immutable_data with 'a @@ portable contended
       The kind of the first is immutable_data with 'a
         because of the definition of t at line 4, characters 2-69.
       But the kind of the first must be a subkind of immutable_data
         with 'a @@ portable contended
         because of the definition of t at line 2, characters 2-58.

       The first mode-crosses less than the second along:
         contention: mod uncontended with 'a ≰ mod uncontended
         portability: mod portable with 'a ≰ mod portable
|}]

module M : sig
  type 'a t : immutable_data with 'a @@ contended with 'a @@ portable
end = struct
  type 'a t : immutable_data with 'a @@ contended portable
end
[%%expect {|
module M : sig type 'a t : immutable_data with 'a end
|}]

module M : sig
  type 'a t : immutable_data with 'a @@ portable
end = struct
  type 'a t : immutable_data with 'a @@ contended portable with 'a @@ portable many
end
[%%expect {|
module M : sig type 'a t : immutable_data with 'a @@ portable end
|}]

type ('a, 'b) u : immutable_data with 'a @@ portable with 'b @@ contended
type ('a, 'b) t : immutable_data with 'a @@ portable with 'b @@ contended = ('a, 'b) u
[%%expect {|
type ('a, 'b) u : immutable_data with 'a @@ portable with 'b @@ contended
type ('a, 'b) t = ('a, 'b) u
|}]

module M : sig
  type ('a, 'b) t : immutable_data with 'a @@ portable with 'b @@ contended
end = struct
  type ('a, 'b) t : immutable_data with 'a @@ contended with 'b @@ portable
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t : immutable_data with 'a @@ contended with 'b @@ portable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('a, 'b) t
             : immutable_data
             with 'a @@ contended

             with 'b @@ portable
         end
       is not included in
         sig
           type ('a, 'b) t
             : immutable_data
             with 'a @@ portable

             with 'b @@ contended
         end
       Type declarations do not match:
         type ('a, 'b) t
           : immutable_data
           with 'a @@ contended

           with 'b @@ portable
       is not included in
         type ('a, 'b) t
           : immutable_data
           with 'a @@ portable

           with 'b @@ contended
       The kind of the first is immutable_data with 'a @@ contended
         with 'b @@ portable
         because of the definition of t at line 4, characters 2-75.
       But the kind of the first must be a subkind of immutable_data
         with 'a @@ portable with 'b @@ contended
         because of the definition of t at line 2, characters 2-75.

       The first mode-crosses less than the second along:
         contention: mod uncontended with 'b ≰ mod uncontended with 'a
         portability: mod portable with 'a ≰ mod portable with 'b
|}]

module M : sig
  type 'a t : immutable_data with 'a @@ portable
end = struct
  type 'a t : immutable_data with 'a @@ portable contended portable
end
[%%expect {|
module M : sig type 'a t : immutable_data with 'a @@ portable end
|}]

type t : immutable_data with int ref @@ contended

module type S = sig
  type t : immutable_data
end

module type T = S with type t = t
[%%expect {|
type t : value mod uncontended
module type S = sig type t : immutable_data end
Line 7, characters 23-33:
7 | module type T = S with type t = t
                           ^^^^^^^^^^
Error: The kind of type "t" is value mod uncontended
         because of the definition of t at line 1, characters 0-49.
       But the kind of type "t" must be a subkind of immutable_data
         because of the definition of t at line 4, characters 2-25.
|}]
