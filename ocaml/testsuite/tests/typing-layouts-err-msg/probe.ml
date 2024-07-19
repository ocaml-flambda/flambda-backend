(* TEST
 setup-ocamlopt.opt-build-env;
 arch_amd64;
 not-macos;
 flags = "-extension layouts_alpha";
 compiler_reference2 = "${test_source_directory}/probe.reference";
 ocamlopt_opt_exit_status = "2";
 compile_only = "true";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

let f (x: float#) = [%probe "a" (
  let f () = x in
  ()
)]
