(* TEST
 setup-ocamlopt.opt-build-env;
 arch_amd64;
 flags = "-extension layouts_alpha";
 compiler_reference2 = "${test_source_directory}/probe.reference";
 compile_only = "true";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

type t : value_or_null

let[@warning "-26"] f (x : t) = [%probe "a" (
  let f () = x in
  ()
)]
