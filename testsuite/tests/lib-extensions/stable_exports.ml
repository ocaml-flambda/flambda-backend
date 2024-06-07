(* TEST
 flags = "-extension-universe stable";
 include stdlib_stable;
 {
   bytecode;
 }{
   native;
 }
*)

open Stdlib_stable

(* Test that [Iarray] is exported. *)
let () =
  let arr = Iarray.init 4 (fun x -> x) in
  assert (Iarray.get arr 2 = 2)
;;

(* Test that [IarrayLabels] is exported. *)
let () =
  let arr = IarrayLabels.init 4 ~f:(fun x -> x) in
  assert (IarrayLabels.get arr 2 = 2)
;;
