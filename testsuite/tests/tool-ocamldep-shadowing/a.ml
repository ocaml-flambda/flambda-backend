(* TEST
 subdirectories = "dir1 dir2";
 setup-ocamlc.byte-build-env;
 commandline = "-depend -slash -I dir1 -I dir2 a.ml";
 compiler_reference = "${test_source_directory}/a.reference";
 {
   ocamlc.byte;
   check-ocamlc.byte-output;
 }
 {
   commandline += " -o a.output";
   ocamlc.byte;
   output = "a.output";
   reference = "${compiler_reference}";
   check-program-output;
 }
*)

include B
include C
