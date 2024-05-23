(* TEST
 flags = "-extension-universe alpha";
 include stdlib_alpha;
 {
   bytecode;
 }{
   native;
 }
*)

open Stdlib_alpha

(* Test that [Or_null] is exported. *)
type ('a : non_null_value) t = 'a Or_null.t
