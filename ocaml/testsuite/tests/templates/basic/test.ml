(* TEST
 readonly_files = "\
   bad_arg_impl.ml bad_arg_impl.reference \
   bad_arg_intf.mli bad_arg_intf.reference \
   bad_instance_arg_name_not_found.ml bad_instance_arg_name_not_found.reference \
   bad_instance_arg_value_not_arg.ml bad_instance_arg_value_not_arg.reference \
   bad_instance_arg_value_not_found.ml bad_instance_arg_value_not_found.reference \
   bad_instance_arg_value_wrong_type.ml bad_instance_arg_value_wrong_type.reference \
   bad_instance_repeated_arg_name.ml bad_instance_repeated_arg_name.reference \
   bad_instance_wrong_mode.ml bad_instance_wrong_mode.reference \
   bad_param_param.mli bad_param_param.reference \
   bad_ref_direct.ml bad_ref_direct.reference \
   bad_ref_direct_imported.ml bad_ref_direct_imported.reference \
   bad_ref_indirect.reference \
   category.ml category.mli \
   category_b.mli \
   category_b_of_category.ml category_b_of_category.mli \
   category_intf.ml \
   category_of_monoid.ml category_of_monoid.mli \
   category_utils.ml category_utils.mli \
   chain.ml chain.mli \
   import.ml \
   import_multi_arg.ml \
   list_element.mli \
   list_monoid.ml list_monoid.mli \
   main.ml main.mli main.reference \
   main_multi_arg.ml main_multi_arg.mli main_multi_arg.reference \
   monoid.mli \
   monoid_of_semigroup.ml monoid_of_semigroup.mli \
   monoid_utils.ml monoid_utils.mli monoid_utils_as_program.reference \
   product_category.ml product_category.mli \
   ref_indirect.ml \
   ref_indirect.cmo.ocamlobjinfo.reference \
   ref_indirect.cmx.ocamlobjinfo.reference \
   semigroup.mli \
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
     flags = "-parameter Monoid -as-parameter";
     module = "bad_param_param.mli";
     compiler_output = "bad_param_param.output";
     ocamlc_byte_exit_status = "2";
     ocamlc.byte;

     compiler_reference = "bad_param_param.reference";
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
     flags = "-as-parameter";
     module = "semigroup.mli";
     ocamlc.byte;

     flags = "";
     module = "category_intf.ml";
     ocamlc.byte;

     flags = "-as-parameter";
     module = "category.mli";
     ocamlc.byte;

     flags = "-parameter Semigroup -as-argument-for Monoid";
     module = "monoid_of_semigroup.mli";
     ocamlc.byte;

     module = "monoid_of_semigroup.ml";
     ocamlc.byte;

     flags = "-as-parameter";
     module = "list_element.mli";
     ocamlc.byte;

     flags = "-parameter List_element -as-argument-for Monoid";
     module = "list_monoid.mli list_monoid.ml";
     ocamlc.byte;

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
     }{
       flags = "-parameter Semigroup";
       module = "bad_instance_repeated_arg_name.ml";
       compiler_output = "bad_instance_repeated_arg_name.output";
       ocamlc_byte_exit_status = "2";
       ocamlc.byte;

       compiler_reference = "bad_instance_repeated_arg_name.reference";
       check-ocamlc.byte-output;
     }{
       flags = "-parameter List_element";
       module = "bad_instance_arg_name_not_found.ml";
       compiler_output = "bad_instance_arg_name_not_found.output";
       ocamlc_byte_exit_status = "2";
       ocamlc.byte;

       compiler_reference = "bad_instance_arg_name_not_found.reference";
       check-ocamlc.byte-output;
     }{
       flags = "-parameter List_element";
       module = "bad_instance_arg_value_not_arg.ml";
       compiler_output = "bad_instance_arg_value_not_arg.output";
       ocamlc_byte_exit_status = "2";
       ocamlc.byte;

       compiler_reference = "bad_instance_arg_value_not_arg.reference";
       check-ocamlc.byte-output;
     }{
       flags = "-parameter List_element";
       module = "bad_instance_arg_value_not_found.ml";
       compiler_output = "bad_instance_arg_value_not_found.output";
       ocamlc_byte_exit_status = "2";
       ocamlc.byte;

       compiler_reference = "bad_instance_arg_value_not_found.reference";
       check-ocamlc.byte-output;
     }{
       flags = "-parameter List_element";
       module = "bad_instance_wrong_mode.ml";
       compiler_output = "bad_instance_wrong_mode.output";
       ocamlc_byte_exit_status = "2";
       ocamlc.byte;

       compiler_reference = "bad_instance_wrong_mode.reference";
       check-ocamlc.byte-output;
     }{
       flags = "-parameter Semigroup";
       module = "bad_ref_direct_imported.ml";
       compiler_output = "bad_ref_direct_imported.output";
       ocamlc_byte_exit_status = "2";
       ocamlc.byte;

       compiler_reference = "bad_ref_direct_imported.reference";
       check-ocamlc.byte-output;
     }{
       flags = "-parameter Category";
       module = "chain.mli chain.ml";
       ocamlc.byte;

       flags = "-parameter Category";
       module = "category_utils.mli category_utils.ml";
       ocamlc.byte;

       flags = "-parameter Monoid -as-argument-for Category";
       module = "category_of_monoid.mli category_of_monoid.ml";
       ocamlc.byte;
       {
         flags = "-parameter List_element";
         module = "bad_instance_arg_value_wrong_type.ml";
         compiler_output = "bad_instance_arg_value_wrong_type.output";
         ocamlc_byte_exit_status = "2";
         ocamlc.byte;

         compiler_reference = "bad_instance_arg_value_wrong_type.reference";
         check-ocamlc.byte-output;
       }{
         flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute";
         module = "import.ml";
         ocamlc.byte;

         {
           flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute";
           module = "main.mli";
           ocamlc.byte;
           {
             flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute -i";
             module = "main.ml";
             ocamlc.byte;

             compiler_reference = "main.reference";
             check-ocamlc.byte-output;
           }{
             module = "main.ml";
             ocamlc.byte;

             program = "main.cmo main.cmi";
             ocamlobjinfo;

             check-program-output;
           }
         }{
           flags = "-as-parameter";
           module = "category_b.mli";
           ocamlc.byte;

           flags = "-parameter Category -as-argument-for Category_b";
           module = "category_b_of_category.mli category_b_of_category.ml";
           ocamlc.byte;

           flags = "-parameter Category -parameter Category_b -as-argument-for Category";
           module = "product_category.mli product_category.ml";
           ocamlc.byte;

           flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute";
           module = "import_multi_arg.ml";
           ocamlc.byte;

           flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute";
           module = "main_multi_arg.mli";
           ocamlc.byte;

           {
             flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute -i";
             module = "main_multi_arg.ml";
             compiler_output = "main_multi_arg.output";
             ocamlc.byte;

             compiler_reference = "main_multi_arg.reference";
             check-ocamlc.byte-output;
           }{
             module = "main_multi_arg.ml";
             ocamlc.byte;
           }
         }
       }
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
     flags = "-parameter Monoid -as-parameter";
     module = "bad_param_param.mli";
     compiler_output = "bad_param_param.output";
     ocamlopt_byte_exit_status = "2";
     ocamlopt.byte;

     compiler_reference = "bad_param_param.reference";
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
     flags = "-as-parameter";
     module = "semigroup.mli";
     ocamlopt.byte;

     flags = "";
     module = "category_intf.ml";
     ocamlc.byte;

     flags = "-as-parameter";
     module = "category.mli";
     ocamlopt.byte;

     flags = "-parameter Semigroup -as-argument-for Monoid";
     module = "monoid_of_semigroup.mli";
     ocamlopt.byte;

     module = "monoid_of_semigroup.ml";
     ocamlopt.byte;

     flags = "-as-parameter";
     module = "list_element.mli";
     ocamlopt.byte;

     flags = "-parameter List_element -as-argument-for Monoid";
     module = "list_monoid.mli list_monoid.ml";
     ocamlopt.byte;

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
       (* Pass [-O2] to normalise ocamlobjinfo output *)
       flags = "-parameter Monoid -O2";
       module = "ref_indirect.ml";
       ocamlopt.byte;

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
     }{
       flags = "-parameter Semigroup";
       module = "bad_instance_repeated_arg_name.ml";
       compiler_output = "bad_instance_repeated_arg_name.output";
       ocamlopt_byte_exit_status = "2";
       ocamlopt.byte;

       compiler_reference = "bad_instance_repeated_arg_name.reference";
       check-ocamlopt.byte-output;
     }{
       flags = "-parameter List_element";
       module = "bad_instance_arg_name_not_found.ml";
       compiler_output = "bad_instance_arg_name_not_found.output";
       ocamlopt_byte_exit_status = "2";
       ocamlopt.byte;

       compiler_reference = "bad_instance_arg_name_not_found.reference";
       check-ocamlopt.byte-output;
     }{
       flags = "-parameter List_element";
       module = "bad_instance_arg_value_not_arg.ml";
       compiler_output = "bad_instance_arg_value_not_arg.output";
       ocamlopt_byte_exit_status = "2";
       ocamlopt.byte;

       compiler_reference = "bad_instance_arg_value_not_arg.reference";
       check-ocamlopt.byte-output;
     }{
       flags = "-parameter List_element";
       module = "bad_instance_arg_value_not_found.ml";
       compiler_output = "bad_instance_arg_value_not_found.output";
       ocamlopt_byte_exit_status = "2";
       ocamlopt.byte;

       compiler_reference = "bad_instance_arg_value_not_found.reference";
       check-ocamlopt.byte-output;
     }{
       flags = "-parameter List_element";
       module = "bad_instance_wrong_mode.ml";
       compiler_output = "bad_instance_wrong_mode.output";
       ocamlopt_byte_exit_status = "2";
       ocamlopt.byte;

       compiler_reference = "bad_instance_wrong_mode.reference";
       check-ocamlopt.byte-output;
     }{
       flags = "-parameter Semigroup";
       module = "bad_ref_direct_imported.ml";
       compiler_output = "bad_ref_direct_imported.output";
       ocamlopt_byte_exit_status = "2";
       ocamlopt.byte;

       compiler_reference = "bad_ref_direct_imported.reference";
       check-ocamlopt.byte-output;
     }{
       flags = "-parameter Category";
       module = "chain.mli chain.ml";
       ocamlopt.byte;

       flags = "-parameter Category";
       module = "category_utils.mli category_utils.ml";
       ocamlopt.byte;

       flags = "-parameter Monoid -as-argument-for Category";
       module = "category_of_monoid.mli category_of_monoid.ml";
       ocamlopt.byte;
       {
         flags = "-parameter List_element";
         module = "bad_instance_arg_value_wrong_type.ml";
         compiler_output = "bad_instance_arg_value_wrong_type.output";
         ocamlopt_byte_exit_status = "2";
         ocamlopt.byte;

         compiler_reference = "bad_instance_arg_value_wrong_type.reference";
         check-ocamlopt.byte-output;
       }{
         flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute";
         module = "import.ml";
         ocamlopt.byte;

         {
           flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute";
           module = "main.mli";
           ocamlopt.byte;
           {
             flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute -i";
             module = "main.ml";
             compiler_output = "main.output";
             ocamlopt.byte;

             compiler_reference = "main.reference";
             check-ocamlopt.byte-output;
           }{
             module = "main.ml";
             ocamlopt.byte;
           }
         }{
           flags = "-as-parameter";
           module = "category_b.mli";
           ocamlopt.byte;

           flags = "-parameter Category -as-argument-for Category_b";
           module = "category_b_of_category.mli category_b_of_category.ml";
           ocamlopt.byte;

           flags = "-parameter Category -parameter Category_b -as-argument-for Category";
           module = "product_category.mli product_category.ml";
           ocamlopt.byte;

           flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute";
           module = "import_multi_arg.ml";
           ocamlopt.byte;

           flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute";
           module = "main_multi_arg.mli";
           ocamlopt.byte;

           {
             flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute -i";
             module = "main_multi_arg.ml";
             compiler_output = "main_multi_arg.output";
             ocamlopt.byte;

             compiler_reference = "main_multi_arg.reference";
             check-ocamlopt.byte-output;
           }{
             module = "main_multi_arg.ml";
             ocamlopt.byte;
           }
         }
       }
     }
   }
 }
*)
