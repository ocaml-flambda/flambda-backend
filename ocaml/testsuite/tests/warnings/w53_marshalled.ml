(* TEST
 readonly_files = "marshall_for_w53.ml w53.ml w53_zero_alloc_all.ml";
 include ocamlcommon;
 setup-ocamlc.byte-build-env;
 program = "${test_build_directory}/marshall_for_w53.exe";
 all_modules = "marshall_for_w53.ml";
 flags = "-w +A-22-27-32-60-67-70-71-72";
 ocamlc.byte;
 run;
 {
   module = "w53.marshalled.ml";
   compiler_reference = "${test_source_directory}/w53.compilers.reference";
   flags = "-w +A-22-27-32-60-67-70-71-72";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }{
   setup-ocamlc.byte-build-env;
   module = "w53_zero_alloc_all.marshalled.ml";
   compiler_reference = "${test_source_directory}/w53_zero_alloc_all.compilers.reference";
   flags = "-w +A-22-27-32-60-67-70-71-72";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }
*)

(* This tests that warning 53 happen appropriately when dealing with marshalled
   ASTs.  It does that by marshalling `w53.ml` to disk and then passing the
   marshalled ast to the compiler. *)
