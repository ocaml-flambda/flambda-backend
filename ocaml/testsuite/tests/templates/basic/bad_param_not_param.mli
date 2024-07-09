(* TEST
 readonly_files = "bad_param_not_param.reference widget.mli";
 setup-ocamlc.byte-build-env;
 flags = "";
 module = "widget.mli";
 compiler_output = "bad_param_not_param.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 flags = "-parameter Widget";
 module = "bad_param_not_param.mli";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "bad_param_not_param.reference";
 check-ocamlc.byte-output;
*)

(* Compiled with [-parameter Widget] but [Widget] is not a parameter *)

val frobnicate : Widget.t -> Widget.t
