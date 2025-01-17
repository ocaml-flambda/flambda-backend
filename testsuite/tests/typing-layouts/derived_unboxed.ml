(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   expect;
 }
*)

open Stdlib_upstream_compatible

module M : sig
  type t_u : value & float64
end = struct
  type t_u = t#
  and t = { s : string ; f : float# }
end
[%%expect{|
module M : sig type t_u : value & float64 end
|}]

type t = { s : s# }
and s = { t : t }
[%%expect{|
type t = { s : s#; }
and s = { t : t; }
|}]

module M = struct
  type t = { i : int }
end
[%%expect{|
module M : sig type t = { i : int; } end
|}]

let x = M.(#{ i = 1 })
[%%expect{|
val x : M.t# = #{i = 1}
|}]

type r = { i : int }
[%%expect{|
type r = { i : int; }
|}]

(**********************************************)
(* Recursion thorugh implicit unboxed records *)

type t = { t : t# }
[%%expect{|
Line 1, characters 0-19:
1 | type t = { t : t# }
    ^^^^^^^^^^^^^^^^^^^
Error: The definition of "t#" is recursive without boxing:
         "t#" contains "t#"
|}]

type t = { t : t# ; i : int }
[%%expect{|
Line 1, characters 0-29:
1 | type t = { t : t# ; i : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "t#" is recursive without boxing:
         "t#" contains "t#"
|}]

type t = { i : int ; t : t# }
[%%expect{|
Line 1, characters 0-29:
1 | type t = { i : int ; t : t# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "t#" is recursive without boxing:
         "t#" contains "t#"
|}]

type t = { s : s# }
and s = { t : t# }
[%%expect{|
Line 1, characters 0-19:
1 | type t = { s : s# }
    ^^^^^^^^^^^^^^^^^^^
Error: The definition of "t#" is recursive without boxing:
         "t#" contains "s#",
         "s#" contains "t#"
|}]

(****************)


(* We give dummy hash types if either the manifest or kind would imply one, so
   the below get good errors. *)

type a = { i : int }
type b = a = { i : int } [@@unboxed]
and s = b#
[%%expect{|
type a = { i : int; }
Line 3, characters 8-10:
3 | and s = b#
            ^^
Error: "b" has no unboxed version. (typedecl.ml)
|}]

type a = { i : int } [@@unboxed]
type b = a = { i : int }
and s = b#
[%%expect{|
type a = { i : int; } [@@unboxed]
Line 3, characters 8-10:
3 | and s = b#
            ^^
Error: "b" has no unboxed version. (typedecl.ml)
|}]
