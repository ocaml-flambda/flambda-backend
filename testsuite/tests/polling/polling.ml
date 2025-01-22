(* TEST
 {
   poll-insertion;
   toplevel.opt;
 }
*)

let[@poll error] loop_alloc x =
  for _ = 0 to x do
    ignore(Sys.opaque_identity(ref 42))
  done
;;

let[@poll error] loop_no_alloc x =
  for _ = 0 to x do
    ignore(Sys.opaque_identity 42)
  done
;;

let[@poll error] rec tailrec x =
  ignore(Sys.opaque_identity(ref 42));
  tailrec x
;;

let[@poll error] rec tailrec_no_alloc x =
  tailrec_no_alloc x
;;
