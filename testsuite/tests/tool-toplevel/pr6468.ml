(* TEST
 toplevel;
*)

(* Make the test reproducible regardless of whether OCAMLRUNPARAM=b or not *)
Printexc.record_backtrace true;;

exception E

let f () = raise E;;
let g () = f (); 1;;
g ();;
