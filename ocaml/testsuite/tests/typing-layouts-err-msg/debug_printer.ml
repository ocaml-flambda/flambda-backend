(* TEST
 flags = "-extension-universe alpha";
 toplevel;
*)
type ('a : float64) t = 'a
let f ppf (x : 'a t) = ();;
#install_printer f;;
(* Always shows the "??? has the wrong type for a printing function" message on exception *)
