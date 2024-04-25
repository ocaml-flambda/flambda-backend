(* TEST
 flags = "-extension-universe alpha";
 toplevel;
*)

type t_any : any;;
(assert false : t_any);;
