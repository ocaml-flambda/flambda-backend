(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type t : value => value
[%%expect {|
type t : value => value
|}]

type ('a : (value, (value) => value) => value) t
[%%expect {|
type ('a : (value, value => value) => value) t
|}]

module M : sig
  type a : value => value
end = struct
  type a : value => value
end
[%%expect {|
module M : sig type a : value => value end
|}]

module M : sig
  type a : value => value
end = struct
  type a : value
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type a : value
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a : value end
       is not included in
         sig type a : value => value end
       Type declarations do not match:
         type a : value
       is not included in
         type a : value => value
       The kind of the first is value
         because of the definition of a at line 4, characters 2-16.
       But the kind of the first must be a subkind of ((value) => value)
         because of the definition of a at line 2, characters 2-25.
|}]

module M : sig
  type a : value
end = struct
  type a : value => value
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type a : value => value
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a : value => value end
       is not included in
         sig type a : value end
       Type declarations do not match:
         type a : value => value
       is not included in
         type a : value
       The kind of the first is ((value) => value)
         because of the definition of a at line 4, characters 2-25.
       But the kind of the first must be a subkind of value
         because of the definition of a at line 2, characters 2-16.
|}]

module M : sig
  type a : (value, value) => value
end = struct
  type a : (value, value) => value
end
[%%expect {|
module M : sig type a : (value, value) => value end
|}]

module M : sig
  type a : (value, value) => value
end = struct
  type a : value => value
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type a : value => value
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a : value => value end
       is not included in
         sig type a : (value, value) => value end
       Type declarations do not match:
         type a : value => value
       is not included in
         type a : (value, value) => value
       The kind of the first is ((value) => value)
         because of the definition of a at line 4, characters 2-25.
       But the kind of the first must be a subkind of
         ((value, value) => value)
         because of the definition of a at line 2, characters 2-34.
|}]

module M : sig
  type a : value => value
end = struct
  type a : value => any
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type a : value => any
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a : value => any end
       is not included in
         sig type a : value => value end
       Type declarations do not match:
         type a : value => any
       is not included in
         type a : value => value
       The kind of the first is ((value) => any)
         because of the definition of a at line 4, characters 2-23.
       But the kind of the first must be a subkind of ((value) => value)
         because of the definition of a at line 2, characters 2-25.
|}]

module M : sig
  type a : value => any
end = struct
  type a : value => value
end
[%%expect {|
module M : sig type a : value => any end
|}]

module M : sig
  type a : any => value
end = struct
  type a : value => value
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type a : value => value
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a : value => value end
       is not included in
         sig type a : any => value end
       Type declarations do not match:
         type a : value => value
       is not included in
         type a : any => value
       The kind of the first is ((value) => value)
         because of the definition of a at line 4, characters 2-25.
       But the kind of the first must be a subkind of ((any) => value)
         because of the definition of a at line 2, characters 2-23.
|}]

module M : sig
  type a : value => value
end = struct
  type a : any => value
end
[%%expect {|
module M : sig type a : value => value end
|}]

module M : sig
  type a : immediate => value
end = struct
  type a : value => value
end
[%%expect {|
module M : sig type a : immediate => value end
|}]

module M : sig
  type a : value => value
end = struct
  type a : immediate => value
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type a : immediate => value
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a : immediate => value end
       is not included in
         sig type a : value => value end
       Type declarations do not match:
         type a : immediate => value
       is not included in
         type a : value => value
       The kind of the first is ((immediate) => value)
         because of the definition of a at line 4, characters 2-29.
       But the kind of the first must be a subkind of ((value) => value)
         because of the definition of a at line 2, characters 2-25.
|}]
