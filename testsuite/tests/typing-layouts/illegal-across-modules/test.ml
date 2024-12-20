(* TEST
   readonly_files = "shadow_stdlib.ml bug.ml ref.ml";
   setup-ocamlc.byte-build-env;
   module = "ref.ml";
   flags = "-allow-illegal-crossing";
   ocamlc.byte;
   module = "shadow_stdlib.ml";
   flags = "";
   ocamlc.byte;
   flags = "-allow-illegal-crossing";
   module = "bug.ml";
   ocamlc.byte;
   check-ocamlc.byte-output;
 *)
