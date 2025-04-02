(* TEST
 readonly_files="monoid_utils.ml monoid_utils.mli monoid.mli";
 {
   setup-ocamlc.byte-build-env;

   flags = "-as-parameter";
   module = "monoid.mli";
   ocamlc.byte;

   flags = "-parameter Monoid";
   module = "monoid_utils.mli monoid_utils.ml";
   ocamlc.byte;

   flags = "-parameter Monoid";
   module = "probe.ml";
   ocamlc.byte;
 }
 {
   setup-ocamlopt.byte-build-env;

   flags = "-as-parameter";
   module = "monoid.mli";
   ocamlopt.byte;

   flags = "-parameter Monoid";
   module = "monoid_utils.mli monoid_utils.ml";
   ocamlopt.byte;

   flags = "-parameter Monoid";
   module = "probe.ml";
   ocamlopt.byte;
 }
*)

let () =
  [%probe "probe" (ignore (Monoid_utils.concat []))];
  print_endline "Hello world!"
