(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type t : value => value = list

[%%expect {|
type t = list
|}]

type it : value = int t
type it' = int t

[%%expect {|
Line 1, characters 18-23:
1 | type it : value = int t
                      ^^^^^
Error: The type constructor t expects 0 argument(s),
       but is here applied to 1 argument(s)
|}]

module M : sig
  type k
end = struct
  type k = int list
end

[%%expect {|
module M : sig type k end
|}]

type t = list

[%%expect {|
Line 1, characters 0-13:
1 | type t = list
    ^^^^^^^^^^^^^
Error: The layout of type list is ((value) => value) (...??)
       But the layout of type list must be a sublayout of any, because
         of the definition of t at line 1, characters 0-13.
|}]

module M : sig
  type t : value => value
end = struct
  type t : value => value = list
end

[%%expect {|
module M : sig type t : (value) => value end
|}]

module M : sig
  type t : (value, value) => value
end = struct
  type t : value => value = list
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : value => value = list
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = list end
       is not included in
         sig type t : (value, value) => value end
       Type declarations do not match:
         type t = list
       is not included in
         type t : (value, value) => value
       The layout of the first is ((value) => value) (...??)
       But the layout of the first must be a sublayout of ((value, value) => value) (...??)
|}]
