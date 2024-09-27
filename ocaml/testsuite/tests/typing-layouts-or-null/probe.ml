(* TEST
 arch_amd64;
 not-macos;
 ocamlopt_flags = "-extension-universe alpha";
 native;
 setup-ocamlopt.opt-build-env;
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

type t : value_or_null

let[@warning "-26"] f (x : t) = [%probe "a" (
  let f () = x in
  ()
)]
