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

(***********************)

(* CR layouts v7.2: Better typechecking of recursive unboxed records is needed
   to give better errors for the following. *)

type t = { t : t# }
[%%expect{|
Line 1, characters 0-19:
1 | type t = { t : t# }
    ^^^^^^^^^^^^^^^^^^^
Error:
       The layout of t# is any
         because a dummy kind of any is used to check mutually recursive datatypes.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of t# must be representable
         because it is the type of record field t.
|}]

type t = { t : t# ; i : int }
[%%expect{|
Line 1, characters 0-29:
1 | type t = { t : t# ; i : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of t# is any & any
         because it is an unboxed record.
       But the layout of t# must be representable
         because it is the type of record field t.
|}]

type t = { i : int ; t : t# }
[%%expect{|
Line 1, characters 0-29:
1 | type t = { i : int ; t : t# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of t# is any & any
         because it is an unboxed record.
       But the layout of t# must be representable
         because it is the type of record field t.
|}]

type t = { s : s# }
and s = { t : t# }
[%%expect{|
Line 1, characters 0-19:
1 | type t = { s : s# }
    ^^^^^^^^^^^^^^^^^^^
Error:
       The layout of t# is any
         because a dummy kind of any is used to check mutually recursive datatypes.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of t# must be representable
         because it is the type of record field t.
|}]
