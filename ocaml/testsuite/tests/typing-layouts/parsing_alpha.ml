(* TEST
 flags = "-extension layouts_alpha";
 toplevel;
*)

type ('a : value) t0 = 'a list;;

type ('a : immediate) t0 = 'a list;;

type ('a : void) t0 = 'a list;;

type ('a : valu) t0 = 'a list;;

type ('a : non_null_value) t0 = 'a;;

type t = float#;;

type t = int#;;

type t = Float.t#;;
