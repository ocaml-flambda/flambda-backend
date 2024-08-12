(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type t : top
[%%expect {|
type t : top
|}]

type t : top => value
[%%expect {|
type t : top => value
|}]

module M : sig
  type t : top
end = struct
  type t : top
end
[%%expect {|
module M : sig type t : top end
|}]

module M : sig
  type t : top
end = struct
  type t : value
end
[%%expect {|
module M : sig type t : top end
|}]

module M : sig
  type t : value => value
end = struct
  type t : value => top
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : value => top
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : value => top end
       is not included in
         sig type t : value => value end
       Type declarations do not match:
         type t : value => top
       is not included in
         type t : value => value
       The kind of the first is ((value) => top)
         because of the definition of t at line 4, characters 2-23.
       But the kind of the first must be a subkind of ((value) => value)
         because of the definition of t at line 2, characters 2-25.
|}]

module M : sig
  type t : value => top
end = struct
  type t : value => value
end
[%%expect {|
module M : sig type t : value => top end
|}]
