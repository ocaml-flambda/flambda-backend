(* TEST
   readonly_files = "foo.mli foo.ml";
   setup-ocamlc.byte-build-env;
   flags = "-c -o bar.cmi";
   module = "foo.mli";
   ocamlc.byte;
   flags = "-c -cmi-file bar.cmi -o bar.cmo";
   module = "foo.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
*)

(* This test is checking that the line number pragmas in [foo.ml] and [foo.mli]
   are sufficient to ensure the tests mention only "bar.{ml,mli}", never
   "foo.*". *)
