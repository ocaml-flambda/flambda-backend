(* TEST_BELOW *)

let[@inline never] test s =
  let s' = (String.cat[@inlined never]) "! " s in
  (Foo.h[@inlined]) s'; (* CR gbury: remove [@inlined], cf #1199 *)
  (Foo.print_stack[@inlined]) (); (* CR gbury: remove [@inlined], cf #1199  *)
  (print_endline[@inlined never]) "end of test"

let () =
  Printexc.record_backtrace true;
  test "foobar"

(* This test aims at checking that backtraces are correct after inlining,
   particularly after inlining of functions from other files, and
   furthermore from other files compiled with different optimization options.
   We therefore define quite a few functions (in `foo.ml`) that use one
   another (with a lot of inlining). Additionally, we also print
   stack/backtraces after an inlined function (in the `test` function above),
   to ensure that we do not wrongly propagate debuginfos past the function
   call. *)

(* TEST
 readonly_files = "foo.ml";
 {
   compiler_directory_suffix = ".O3";
   setup-ocamlopt.opt-build-env;
   module = "foo.ml";
   flags = "-g -O3";
   ocamlopt.opt;
   module = "main.ml";
   flags = "-g -O3";
   ocamlopt.opt;
   module = "";
   all_modules = "foo.cmx main.cmx";
   ocamlopt.opt;
   run;
   check-program-output;
 }{
   compiler_directory_suffix = ".Oclassic";
   setup-ocamlopt.opt-build-env;
   module = "foo.ml";
   flags = "-g -Oclassic";
   ocamlopt.opt;
   module = "main.ml";
   flags = "-g -Oclassic";
   ocamlopt.opt;
   module = "";
   all_modules = "foo.cmx main.cmx";
   ocamlopt.opt;
   run;
   check-program-output;
 }{
   compiler_directory_suffix = ".O3-Oclassic";
   setup-ocamlopt.opt-build-env;
   module = "foo.ml";
   flags = "-g -O3";
   ocamlopt.opt;
   module = "main.ml";
   flags = "-g -Oclassic";
   ocamlopt.opt;
   module = "";
   all_modules = "foo.cmx main.cmx";
   ocamlopt.opt;
   run;
   check-program-output;
 }{
   compiler_directory_suffix = ".Oclassic-O3";
   setup-ocamlopt.opt-build-env;
   module = "foo.ml";
   flags = "-g -Oclassic";
   ocamlopt.opt;
   module = "main.ml";
   flags = "-g -O3";
   ocamlopt.opt;
   module = "";
   all_modules = "foo.cmx main.cmx";
   ocamlopt.opt;
   run;
   check-program-output;
 }
*)
