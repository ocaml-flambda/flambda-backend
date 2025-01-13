(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* Non-existent #-type *)
type t = a#
[%%expect{|
Line 1, characters 9-11:
1 | type t = a#
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
Line 1, characters 9-11:
1 | type u = t#
             ^^
Error: "t" has no unboxed version.
|}]

(* Shadowing hides #-type *)
type float
[%%expect{|
type float
|}]

(* But it's still accessible via the alias *)
type u2 = t#
[%%expect{|
Line 1, characters 10-12:
1 | type u2 = t#
              ^^
Error: "t" has no unboxed version.
|}]

module M = struct
  type t = int32
end
[%%expect{|
module M : sig type t = int32 end
|}]

type int32_u = M.t#
[%%expect{|
Line 1, characters 15-19:
1 | type int32_u = M.t#
                   ^^^^
Error: "M.t" has no unboxed version.
|}]
