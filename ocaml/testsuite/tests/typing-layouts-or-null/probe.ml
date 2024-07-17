(* TEST
 setup-ocamlopt.opt-build-env;
 arch_amd64;
 flags = "-extension layouts_alpha";
 compile_only = "true";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

type t : value_or_null

let[@warning "-26"] f (x : t) = [%probe "a" (
  let f () = x in
  ()
)]
