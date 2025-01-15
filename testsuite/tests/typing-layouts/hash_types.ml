(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* Non-existent #-type *)
type bad = a#
[%%expect{|
Line 1, characters 11-13:
1 | type bad = a#
               ^^
Error: Unbound type constructor "a"
|}]

type t = float#
[%%expect{|
type t = float#
|}]

type t = float
[%%expect{|
type t = float
|}]

(* Alias gets its own #-type *)
(* CR rtjoa:  *)
type u = t#
[%%expect{|
type u = t#
|}]

(* Shadowing hides #-type *)
type float
[%%expect{|
type float
|}]

type bad = float#

(* But it's still accessible via the alias *)
type u2 = t#
[%%expect{|
Line 1, characters 11-17:
1 | type bad = float#
               ^^^^^^
Error: "float" has no unboxed version.
|}]

module M = struct
  type t = int32
end
[%%expect{|
module M : sig type t = int32 end
|}]

type int32_u = M.t#
[%%expect{|
type int32_u = M.t#
|}]

(* Restore float *)
type float = t
[%%expect{|
type float = t
|}]

module type S = sig
  type t = float
  type u = t#
end with type t := float
[%%expect{|
module type S = sig type u = float# end
|}]

module S : sig
  type t = float
  type u = t#
  val f : u -> t#
end with type u := float# = struct
  type t = float
  let f x = x
end
[%%expect{|
module S : sig type t = float val f : float# -> t# end
|}]

type 'a t = float
type s = int t#
type v = string t#

let (_ : s -> v) = fun x -> x
[%%expect{|
type 'a t = float
type s = int t#
type v = string t#
- : t/2# -> t/2# = <fun>
|}, Principal{|
type 'a t = float
type s = int t#
type v = string t#
- : s -> v = <fun>
|}]

(* The difference between principal and non-principal above is normal. See: *)

type 'a bar = float
type intbar = int bar
type stringbar = string bar
let (_ : intbar * intbar -> stringbar * stringbar) = fun x -> x
[%%expect{|
type 'a bar = float
type intbar = int bar
type stringbar = string bar
- : stringbar * stringbar -> stringbar * stringbar = <fun>
|}, Principal{|
type 'a bar = float
type intbar = int bar
type stringbar = string bar
- : intbar * intbar -> stringbar * stringbar = <fun>
|}]

type t = float
and s = t#
[%%expect{|
Line 2, characters 8-10:
2 | and s = t#
            ^^
Error: "t" has no unboxed version.
|}]
