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

(* Test that [Capsule] is exported. *)
let () =
  let x = Capsule.Ptr.inject 5 in
  assert (Capsule.Ptr.project x = 5)
;;

(* Test that [Or_null] is exported. *)

let () =
  match Or_null.null with
  | Null -> ()
  | This _ -> assert false
;;
