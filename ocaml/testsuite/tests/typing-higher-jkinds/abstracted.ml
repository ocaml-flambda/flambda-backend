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
module M : sig type t : value => value end
|}]
