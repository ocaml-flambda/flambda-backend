(* TEST
 readonly_files = "\
   bad_arg_impl.ml bad_arg_impl.reference \
   bad_arg_intf.mli bad_arg_intf.reference \
   bad_ref_direct.ml bad_ref_direct.reference \
   bad_ref_indirect.reference \
   monoid.mli \
   monoid_utils.ml monoid_utils.mli monoid_utils_as_program.reference \
   ref_indirect.ml \
   ref_indirect.cmo.ocamlobjinfo.reference \
   ref_indirect.cmx.ocamlobjinfo.reference \
   string_monoid.ml string_monoid.mli \
   test_direct_access.ml test_direct_access.reference \
 ";

 {
   setup-ocamlc.byte-build-env;

   flags = "-as-parameter";
   module = "monoid.mli";
   ocamlc.byte;
   {
     flags = "";
     module = "bad_ref_direct.ml";
     compiler_output = "bad_ref_direct.output";
     ocamlc_byte_exit_status = "2";
     ocamlc.byte;

     compiler_reference = "bad_ref_direct.reference";
     check-ocamlc.byte-output;
   }{
     flags = "-as-argument-for Monoid";
     module = "bad_arg_impl.ml";
     compiler_output = "bad_arg_impl.output";
     ocamlc_byte_exit_status = "2";
     ocamlc.byte;

     compiler_reference = "bad_arg_impl.reference";
     check-ocamlc.byte-output;
   }{
     flags = "-as-argument-for Monoid";
     module = "bad_arg_intf.mli";
     compiler_output = "bad_arg_intf.output";
     ocamlc_byte_exit_status = "2";
     ocamlc.byte;

     compiler_reference = "bad_arg_intf.reference";
     check-ocamlc.byte-output;
   }{
     src = "string_monoid.ml";
     dst = "string_monoid_no_mli.ml";
     copy;

     flags = "-as-argument-for Monoid";
     module = "string_monoid_no_mli.ml string_monoid.mli string_monoid.ml";
     ocamlc.byte;

     flags = "";
     module = "test_direct_access.ml";
     ocamlc.byte;

     flags = "";
     program = "${test_build_directory}/test_direct_access.bc";
     module = "";
     all_modules = "\
       string_monoid.cmo \
       string_monoid_no_mli.cmo \
       test_direct_access.cmo \
     ";
     ocamlc.byte;

     output = "test_direct_access.output";
     exit_status = "0";
     run;

     reference = "test_direct_access.reference";
     check-program-output;
   }{
     flags = "-parameter Monoid";
     module = "monoid_utils.mli monoid_utils.ml";
     ocamlc.byte;
     {
       src = "ref_indirect.ml";
       dst = "bad_ref_indirect.ml";
       copy;

       flags = "";
       module = "bad_ref_indirect.ml";
       compiler_output = "bad_ref_indirect.output";
       ocamlc_byte_exit_status = "2";
       ocamlc.byte;

       compiler_reference = "bad_ref_indirect.reference";
       check-ocamlc.byte-output;
     }{
       flags = "-parameter Monoid";
       module = "ref_indirect.ml";
       ocamlc.byte;

       (* [-no-code] and [-no-approx] are currently unimplemented (see PR 2737), which
          sadly does make the reference file here a mite bloated and sensitive to
          random changes in flambda2. *)
       program = "-no-code -no-approx ref_indirect.cmo ref_indirect.cmi";
       output = "ref_indirect.cmo.ocamlobjinfo.output";
       ocamlobjinfo;

       reference = "ref_indirect.cmo.ocamlobjinfo.reference";
       check-program-output;
     }{
       program = "${test_build_directory}/monoid_utils_as_program.bc";
       module = "";
       all_modules = "monoid_utils.cmo ";
       ocamlc.byte;

       output = "monoid_utils_as_program.output";
       exit_status = "2";
       run;

       reference = "monoid_utils_as_program.reference";
       check-program-output;
     }
   }
 }{
   setup-ocamlopt.byte-build-env;

   flags = "-as-parameter";
   module = "monoid.mli";
   ocamlopt.byte;
   {
     flags = "";
     module = "bad_ref_direct.ml";
     compiler_output = "bad_ref_direct.output";
     ocamlopt_byte_exit_status = "2";
     ocamlopt.byte;

     compiler_reference = "bad_ref_direct.reference";
     check-ocamlopt.byte-output;
   }{
     flags = "-as-argument-for Monoid";
     module = "bad_arg_impl.ml";
     compiler_output = "bad_arg_impl.output";
     ocamlopt_byte_exit_status = "2";
     ocamlopt.byte;

     compiler_reference = "bad_arg_impl.reference";
     check-ocamlopt.byte-output;
   }{
     flags = "-as-argument-for Monoid";
     module = "bad_arg_intf.mli";
     compiler_output = "bad_arg_intf.output";
     ocamlopt_byte_exit_status = "2";
     ocamlopt.byte;

     compiler_reference = "bad_arg_intf.reference";
     check-ocamlopt.byte-output;
   }{
     src = "string_monoid.ml";
     dst = "string_monoid_no_mli.ml";
     copy;

     flags = "-as-argument-for Monoid";
     module = "string_monoid_no_mli.ml string_monoid.mli string_monoid.ml";
     ocamlopt.byte;

     flags = "";
     module = "test_direct_access.ml";
     ocamlopt.byte;

     flags = "";
     program = "${test_build_directory}/test_direct_access.exe";
     module = "";
     all_modules = "\
       string_monoid.cmx \
       string_monoid_no_mli.cmx \
       test_direct_access.cmx \
     ";
     ocamlopt.byte;

     output = "test_direct_access.output";
     exit_status = "0";
     run;

     reference = "test_direct_access.reference";
     check-program-output;
   }{
     flags = "-parameter Monoid";
     module = "monoid_utils.mli monoid_utils.ml";
     ocamlopt.byte;
     {
       src = "ref_indirect.ml";
       dst = "bad_ref_indirect.ml";
       copy;

       flags = "";
       module = "bad_ref_indirect.ml";
       compiler_output = "bad_ref_indirect.output";
       ocamlopt_byte_exit_status = "2";
       ocamlopt.byte;

       compiler_reference = "bad_ref_indirect.reference";
       check-ocamlopt.byte-output;
     }{
       flags = "-parameter Monoid";
       module = "ref_indirect.ml";
       ocamlopt.byte;

       (* [-no-code] and [-no-approx] are currently unimplemented (see PR 2737), which
          sadly does make the reference file here a mite bloated and sensitive to
          random changes in flambda2. *)
       program = "-no-code -no-approx ref_indirect.cmx ref_indirect.cmi";
       output = "ref_indirect.cmx.ocamlobjinfo.output";
       ocamlobjinfo;

       reference = "ref_indirect.cmx.ocamlobjinfo.reference";
       check-program-output;
     }{
       program = "${test_build_directory}/monoid_utils_as_program.exe";
       module = "";
       all_modules = "monoid_utils.cmx ";
       ocamlopt.byte;

       output = "monoid_utils_as_program.output";
       exit_status = "2";
       run;

       reference = "monoid_utils_as_program.reference";
       check-program-output;
     }
   }
 }
*)
