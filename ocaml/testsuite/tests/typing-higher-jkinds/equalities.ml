(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type t : value => value

[%%expect {|
type t : (value) => value
|}]

module M : sig
  type a : value => value
end = struct
  type a : value => value
end

[%%expect {|
module M : sig type a : (value) => value end
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
         sig type a : (value) => value end
       Type declarations do not match:
         type a : value
       is not included in
         type a : (value) => value
       The layout of the first is value, because
         of the definition of a at line 4, characters 2-16.
       But the layout of the first must be a sublayout of ((value) => value) (...??)
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
         sig type a : (value) => value end
       is not included in
         sig type a : value end
       Type declarations do not match:
         type a : (value) => value
       is not included in
         type a : value
       The layout of the first is ((value) => value) (...??)
       But the layout of the first must be a sublayout of value, because
         of the definition of a at line 2, characters 2-16.
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
         sig type a : (value) => value end
       is not included in
         sig type a : (value, value) => value end
       Type declarations do not match:
         type a : (value) => value
       is not included in
         type a : (value, value) => value
       The layout of the first is ((value) => value) (...??)
       But the layout of the first must be a sublayout of ((value, value) => value) (...??)
|}]
