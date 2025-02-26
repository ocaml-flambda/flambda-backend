(* TEST
 flags = "-extension-universe beta";
 include stdlib_beta;
 {
   bytecode;
 }{
   native;
 }
*)

open Stdlib_beta

(* Test that [Or_null] is exported. *)

let () =
  match Or_null.null with
  | Null -> ()
  | This _ -> assert false
;;
