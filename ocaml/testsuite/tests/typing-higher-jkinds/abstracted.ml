(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

module M : sig
  type t : value => value
end = struct
  type 'a t = { foo : 'a }
end
[%%expect {|
module M : sig type t : value => value end
|}]

module M : sig
  type t : value => value
end = struct
  type 'a t = ('a * 'a) list
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = ('a * 'a) list
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = ('a * 'a) list end
       is not included in
         sig type t : value => value end
       Type declarations do not match:
         type 'a t = ('a * 'a) list
       is not included in
         type t : value => value
       They have different arities.
|}]

module M : sig
  type t : (value, value) => value
end = struct
  type ('a, 'b) t = { a : 'a; b : 'b }
end
[%%expect {|
module M : sig type t : (value, value) => value end
|}]

module M : sig
  type t : (value, value) => value
end = struct
  type ('a, 'b) t
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t end
       is not included in
         sig type t : (value, value) => value end
       Type declarations do not match:
         type ('a, 'b) t
       is not included in
         type t : (value, value) => value
       They have different arities.
|}]

module M : sig
  type t : (value, value) => value
end = struct
  type ('a, 'b) t [@@datatype]
end
[%%expect {|
module M : sig type t : (value, value) => value end
|}]

module M : sig
  type t : value => value
end = struct
  type t : value => value
end
[%%expect {|
module M : sig type t : value => value end
|}]

module M : sig
  type t : value => value
end = struct
  type 'a t [@@datatype]
end
[%%expect {|
module M : sig type t : value => value end
|}]

module M : sig
  type 'a t
end = struct
  type 'a t [@@datatype]
end
[%%expect {|
module M : sig type 'a t end
|}]

module M : sig
  type 'a t [@@datatype]
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
         sig type 'a t end
       Type declarations do not match: type 'a t is not included in type 'a t
       The first is abstract, but the second is an abstract datatype.
|}]

module M : sig
  type ('a : any) t [@@datatype]
end = struct
  type t : any => value = array
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : any => value = array
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = array end
       is not included in
         sig type ('a : any) t end
       Type declarations do not match:
         type t = array
       is not included in
         type ('a : any) t
       The first is abstract, but the second is an abstract datatype.
|}]

module M : sig
  type t : any => value
end = struct
  type t : any => value = array
end
[%%expect {|
module M : sig type t : any => value end
|}]

module M : sig
  type t : any => value
end = struct
  type s : any => value
  type t : any => value = s
end
[%%expect {|
module M : sig type t : any => value end
|}]

module M : sig
  type t : any => value
end = struct
  type ('a : any) s [@@datatype]
  type t : any => value = s
end
[%%expect {|
module M : sig type t : any => value end
|}]

module M : sig
  type t : any => value
end = struct
  type ('a : any) s
  type t : any => value = s
end
[%%expect {|
Line 5, characters 26-27:
5 |   type t : any => value = s
                              ^
Error: The type constructor s expects 1 argument(s),
       but is here applied to 0 argument(s)
|}]

module M : sig
  type t : any => value
end = struct
  type ('a : any) s
  type ('a : any) t = 'a s
end
[%%expect {|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   type ('a : any) s
5 |   type ('a : any) t = 'a s
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a : any) s type ('a : any) t = 'a s end
       is not included in
         sig type t : any => value end
       Type declarations do not match:
         type ('a : any) t = 'a s
       is not included in
         type t : any => value
       They have different arities.
|}]

type 'a t = 'a * 'a
type 'a s = 'a t [@@datatype]
[%%expect {|
type 'a t = 'a * 'a
Line 2, characters 0-29:
2 | type 'a s = 'a t [@@datatype]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This is not a datatype, but one was expected.
|}]
