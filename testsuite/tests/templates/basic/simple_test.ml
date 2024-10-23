(* TEST

 readonly_files = "\
   monoid.mli \
   monoid_utils.ml monoid_utils.mli \
   string_monoid.ml string_monoid.mli \
 ";

 setup-ocamlopt.byte-build-env;

 flags = "-as-parameter";
 module = "monoid.mli";
 ocamlopt.byte;

 flags = "-as-argument-for Monoid -dlambda";
 module = "string_monoid.mli string_monoid.ml";
 ocamlopt.byte;

 flags = "-parameter Monoid -dlambda";
 module = "monoid_utils.mli monoid_utils.ml";
 ocamlopt.byte;

 flags = "-instantiate -dlambda -dcmm";
 module = "";
 program = "monoid_utils-String_monoid.cmx";
 all_modules = "monoid_utils.cmx string_monoid.cmx";
 ocamlopt.byte;

 flags = "-w -misplaced-attribute -dlambda";
 module = "simple_test.ml";
 program = "";
 all_modules = "";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "simple_test.exe";
 all_modules = "\
   string_monoid.cmx \
   monoid_utils.cmx \
   monoid_utils-String_monoid.cmx \
   simple_test.cmx \
 ";
 ocamlopt.byte;

 program = "${test_build_directory}/simple_test.exe";
 run;

 check-program-output;
*)

module M =
  Monoid_utils(Monoid)(String_monoid) [@jane.non_erasable.instances]

let () = print_endline (M.concat ["Hello "; "world"; "!"])
