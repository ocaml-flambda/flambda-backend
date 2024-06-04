(* TEST
 readonly_files = "w53_ppx.ml";
 include ocamlcommon;
 setup-ocamlc.byte-build-env;
 program = "${test_build_directory}/w53_ppx.exe";
 all_modules = "w53_ppx.ml";
 ocamlc.byte;
 module = "w53_with_ppx.ml";
 flags = "-ppx ${program}";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* This test checks that compiler-builtin attributes inserted by a ppx still
   trigger the misplaced attribute warning if they are unused (and not if
   they are used). *)

let x = 3 [@@test]

type t = int [@@test]
