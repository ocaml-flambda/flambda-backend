(* TEST
 readonly_files = "main.ml member.ml";
 {
   setup-ocamlc.byte-build-env;
   flags = "-for-pack Pack";
   module = "member.ml";
   ocamlc.byte;
   module = "";
   flags = "-pack";
   program = "pack.cmo";
   all_modules = "member.cmo";
   ocamlc.byte;
   flags = "";
   module = "main.ml";
   ocamlc.byte;
   module = "";
   program = "${test_build_directory}/main.byte";
   all_modules = "pack.cmo main.cmo";
   ocamlc.byte;
   exit_status = "0";
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   flags = "-for-pack Pack";
   module = "member.ml";
   ocamlopt.byte;
   module = "";
   flags = "-pack";
   program = "pack.cmx";
   all_modules = "member.cmx";
   ocamlopt.byte;
   flags = "";
   module = "main.ml";
   ocamlopt.byte;
   module = "";
   program = "${test_build_directory}/main.exe";
   all_modules = "pack.cmx main.cmx";
   ocamlopt.byte;
   exit_status = "0";
   run;
   check-program-output;
 }
*)

let say_hello () =
  Pack.Member.say_hello ()

let () =
  say_hello ();

  let ctor = Obj.Extension_constructor.of_val Pack.Member.A in
  print_endline (Obj.Extension_constructor.name ctor)

