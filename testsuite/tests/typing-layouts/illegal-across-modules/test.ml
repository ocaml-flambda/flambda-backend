(* TEST
   readonly_files = "shadow_stdlib.ml bug.ml";
   setup-ocamlc.byte-build-env;
   module = "shadow_stdlib.ml";
   ocamlc.byte;
   flags = "-allow-illegal-crossing";
   module = "bug.ml";
   ocamlc.byte;
   check-ocamlc.byte-output;
 *)
