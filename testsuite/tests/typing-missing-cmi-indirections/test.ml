(* TEST
 readonly_files = "missing.mli present.mli client.ml";
 setup-ocamlc.byte-build-env;
 module = "missing.mli";
 ocamlc.byte;
 flags = "-open Missing";
 module = "present.mli";
 ocamlc.byte;
 script = "rm missing.cmi";
 script;
 flags = "-c -i";
 module = "client.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
