(* TEST
 readonly_files = "gadt_extensible.ml insert_extensible.ml extract_extensible.ml";
 flags = "-extension-universe alpha";
 setup-ocamlc.byte-build-env;
 module = "gadt_extensible.ml";
 ocamlc.byte;
 module = "extract_extensible.ml";
 ocamlc.byte;
 module = "insert_extensible.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
