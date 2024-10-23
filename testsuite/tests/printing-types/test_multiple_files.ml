(* TEST
 readonly_files = "multiple_files.ml";
 setup-ocamlopt.opt-build-env;
 module = "multiple_files.ml";
 flags = "-g";
 ocamlopt.opt;
 module = "test_multiple_files.ml";
 flags = "-short-paths -i";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* Ensure that underscore-prefixed type names
   are avoided by the type printer when
   a good, underscoreless name is available.
*)

let other_file_b = Multiple_files.B1

module A = struct
  type t = A1 | A2
end

type _a = A.t = A1 | A2

let this_file_a = A1
