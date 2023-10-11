(* TEST
   flags = "-extension layouts_alpha"
   * toplevel
*)

type ('a : value) t0 = 'a list;;

type ('a : immediate) t0 = 'a list;;

type ('a : void) t0 = 'a list;;

type ('a : valu) t0 = 'a list;;

type t = float#;;

type t = int#;;

type t = Float.t#;;
