(* TEST
 flags = "-flambda2-advanced-meet";
 flambda2;
 native;
*)

type _ opt_or_string =
 | S : string opt_or_string
 | O : string option opt_or_string

let to_string (type a) (x : a opt_or_string) (y : a) : string =
  match x, y with
  | S, s -> s
  | O, None -> ""
  | O, Some s -> s

let test () =
  to_string (Sys.opaque_identity S) "foo"
