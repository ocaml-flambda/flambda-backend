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

let () =
  match Or_null.null with
  | Null -> ()
  | This _ -> assert false
;;
