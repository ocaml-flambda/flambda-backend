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
  let x = Capsule.Data.inject 5 in
  assert (Capsule.Data.project x = 5)
;;
