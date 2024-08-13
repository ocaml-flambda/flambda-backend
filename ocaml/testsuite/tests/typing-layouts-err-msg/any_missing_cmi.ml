(* TEST
 readonly_files = "any_missing_cmi_lib.ml any_missing_cmi_lib2.ml";
 setup-ocamlc.byte-build-env;
 module = "any_missing_cmi_lib2.ml";
 ocamlc.byte;
 module = "any_missing_cmi_lib.ml";
 ocamlc.byte;
 script = "rm -f any_missing_cmi_lib2.cmi";
 script;
 expect;
*)

#directory "ocamlc.byte";;
#load "any_missing_cmi_lib.cmo";;

let f = Any_missing_cmi_lib.f (assert false)
[%%expect{|
Unknown directive "directory".
|}]
