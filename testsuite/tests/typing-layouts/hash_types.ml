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
  type u = t#
end
[%%expect{|
module M : sig type t = int32 type u = t# end
|}]

type int32_u = M.t#
[%%expect{|
Line 1, characters 15-19:
1 | type int32_u = M.t#
                   ^^^^
Error: "M.t" has no unboxed version.
|}]
