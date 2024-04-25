(* TEST
 flags = "-extension-universe alpha";
 toplevel;
*)

type t_void : void;;
(assert false : t_void);;
