(* TEST
 include dynlink;
 libraries = "";
 readonly_files = "a.ml b.ml loader.ml";
 shared-libraries;
 {
   setup-ocamlc.byte-build-env;
   {
     flags = "-for-pack Packed";
     module = "a.ml";
     ocamlc.byte;
   }{
     flags = "-for-pack Packed";
     module = "b.ml";
     ocamlc.byte;
   }{
     program = "packed.cmo";
     flags = "-pack";
     all_modules = "a.cmo b.cmo";
     ocamlc.byte;
   }{
     program = "${test_build_directory}/loader.byte";
     flags = "-linkall";
     include ocamlcommon;
     libraries += "dynlink";
     all_modules = "loader.ml";
     ocamlc.byte;
     arguments = "packed.cmo";
     exit_status = "0";
     run;
     reference = "${test_source_directory}/byte.reference";
     check-program-output;
   }
 }{
   native-dynlink;
   setup-ocamlopt.byte-build-env;
   {
     flags = "-for-pack Packed";
     module = "a.ml";
     ocamlopt.byte;
   }{
     flags = "-for-pack Packed";
     module = "b.ml";
     ocamlopt.byte;
   }{
     program = "packed.cmx";
     flags = "-pack";
     all_modules = "a.cmx b.cmx";
     ocamlopt.byte;
   }{
     program = "plugin.cmxs";
     flags = "-shared";
     all_modules = "packed.cmx";
     ocamlopt.byte;
   }{
     program = "${test_build_directory}/loader.exe";
     flags = "-linkall";
     include ocamlcommon;
     libraries += "dynlink";
     all_modules = "loader.ml";
     ocamlopt.byte;
     arguments = "plugin.cmxs";
     exit_status = "0";
     run;
     reference = "${test_source_directory}/native.reference";
     check-program-output;
   }
 }
*)
let () =
  try
    Dynlink.loadfile Sys.argv.(1)
  with
  | Dynlink.Error error ->
    prerr_endline (Dynlink.error_message error)
