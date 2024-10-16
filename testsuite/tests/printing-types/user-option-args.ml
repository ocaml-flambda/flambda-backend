(* TEST
 toplevel;
*)

(** Test that type of optional argument is printed, even if using a non-predef
    option type *)

type nonrec 'a maybe = 'a option;;

let create_property ?x () = let _ =  (x : int maybe) in ();;
