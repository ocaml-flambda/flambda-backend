(* TEST
 readonly_files = "pr2978.ml pr2978.mli";
setup-ocamlc.byte-build-env;
module = "pr2978.mli";
ocamlc.byte;
module = "pr2978.ml";
ocamlc.byte;
check-ocamlc.byte-output;
*)

let[@tail_mod_cons]  f x = (42, 42)
