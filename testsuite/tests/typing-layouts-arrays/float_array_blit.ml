(* TEST
 flags = "-extension layouts_beta";
 native;
*)

external[@layout_poly] unsafe_blit :
  ('a : any_non_null) . ('a array[@local_opt]) -> (int[@local_opt]) -> ('a array[@local_opt]) -> (int[@local_opt]) -> (int[@local_opt]) -> unit =
  "%arrayblit"

let a : float array = [| 0. ; 1. |];;

unsafe_blit a 0 a 1 1;;
