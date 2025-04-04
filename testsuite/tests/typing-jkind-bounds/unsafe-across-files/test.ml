(* TEST
   readonly_files = "unsafe_across_files.mli unsafe_across_files.ml";
   setup-ocamlc.byte-build-env;
   module = "unsafe_across_files.mli";
   ocamlc.byte;
   module = "unsafe_across_files.ml";
   ocamlc.byte;
*)
