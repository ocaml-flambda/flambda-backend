(* TEST
 readonly_files = "pr2979.ml pr2979.mli";
setup-ocamlc.byte-build-env;
module = "pr2979.mli";
ocamlc.byte;
module = "pr2979.ml";
ocamlc_byte_exit_status = "2";
ocamlc.byte;
check-ocamlc.byte-output;
*)

let[@tail_mod_cons]  f x = (42, 42)
