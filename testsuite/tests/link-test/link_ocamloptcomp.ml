(* TEST
 {
   setup-ocamlc.byte-build-env;
   module = "link_ocamloptcomp.ml";
   ocamlc.byte with ocamlcommon;
   module = "";
   flags = "-linkall";
   all_modules = "";
   program = "link_ocamloptcomp.cma";
   libraries += "ocamloptcomp";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }{
   setup-ocamlopt.byte-build-env;
   module = "link_ocamloptcomp.ml";
   ocamlopt.byte with ocamlcommon;
   module = "";
   flags = "-linkall";
   all_modules = "";
   program = "link_ocamloptcomp.cmxa";
   libraries += "ocamloptcomp";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }
*)
(* If this test fails, a new library likely needs to be added to the
   ocamloptcomp_with_flambda2 rules in the top-level dune file. *)
