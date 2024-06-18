(* TEST
 readonly_files = "\
   bad_arg_impl.ml bad_arg_impl.reference \
   bad_arg_intf.mli bad_arg_intf.reference \
   bad_instance_arg_name_not_found.ml bad_instance_arg_name_not_found.reference \
   bad_instance_arg_value_not_arg.ml bad_instance_arg_value_not_arg.reference \
   bad_instance_arg_value_not_found.ml bad_instance_arg_value_not_found.reference \
   bad_instance_arg_value_wrong_type.ml bad_instance_arg_value_wrong_type.reference \
   bad_instantiate_not_parameterised_cmo.reference \
   bad_instantiate_not_parameterised_cmx.reference \
   bad_ref_direct.ml bad_ref_direct.reference \
   bad_ref_direct_imported.ml bad_ref_direct_imported.reference \
   bad_ref_indirect.ml bad_ref_indirect.reference \
   category.ml category.mli \
   category_of_monoid.ml category_of_monoid.mli \
   category_utils.ml category_utils.mli \
   chain.ml chain.mli \
   import.ml \
   int_list_element.ml int_list_element.mli \
   list_element.mli \
   list_monoid.ml list_monoid.mli \
   main.ml main.mli main.reference main-ocamlobjinfo.reference \
   monoid.mli \
   monoid_of_semigroup.ml monoid_of_semigroup.mli \
   monoid_utils.ml monoid_utils.mli monoid_utils_as_program.reference \
   run.ml \
   semigroup.mli \
   string_monoid.ml string_monoid.mli \
   string_semigroup.ml string_semigroup.mli \
   test.reference \
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
     module = "semigroup.mli";
     ocamlc.byte;

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
       flags = "";
       module = "bad_ref_indirect.ml";
       compiler_output = "bad_ref_indirect.output";
       ocamlc_byte_exit_status = "2";
       ocamlc.byte;

       compiler_reference = "bad_ref_indirect.reference";
       check-ocamlc.byte-output;
     }{
       (* Linking an uninstantiated parameterised module is weird but should be harmless
          (it's just a module that exports a single functor). We should probably warn in
          this case, since it's probably not what the user meant to do. *)

       program = "${test_build_directory}/monoid_utils_as_program.bc";
       module = "";
       all_modules = "monoid_utils.cmo ";
       ocamlc.byte;

       output = "monoid_utils_as_program.output";
       run;

       reference = "monoid_utils_as_program.reference";
       check-program-output;
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

         flags = "-as-argument-for Semigroup";
         module = "string_semigroup.mli";
         ocamlc.byte;

         module = "string_semigroup.ml";
         ocamlc.byte;

         module = "";
         flags = "-instantiate";
         program = "monoid_of_semigroup-String_semigroup.cmo";
         all_modules = "monoid_of_semigroup.cmo string_semigroup.cmo";
         ocamlc.byte;

         flags = "-as-argument-for List_element";
         module = "int_list_element.mli int_list_element.ml";
         ocamlc.byte;

         (* CR lmaurer: I'm indenting everything after these [bad_instantiate_*] tests
            under protest. See PR 2693 for a better way. *)

         {
           module = "";
           flags = "-instantiate";
           program = "int_list_element-String_semigroup.cmo";
           all_modules = "int_list_element.cmo string_semigroup.cmo";
           compiler_output = "bad_instantiate_not_parameterised_cmo.output";
           ocamlc_byte_exit_status = "2";
           ocamlc.byte;

           compiler_reference = "bad_instantiate_not_parameterised_cmo.reference";
           check-ocamlc.byte-output;
         }{
           module = "";
           flags = "-instantiate -as-argument-for Monoid";
           program = "list_monoid-Int_list_element.cmo";
           all_modules = "list_monoid.cmo int_list_element.cmo";
           ocamlc.byte;

           module = "";
           flags = "-instantiate -as-argument-for Monoid";
           program = "monoid_of_semigroup-String_semigroup.cmo";
           all_modules = "monoid_of_semigroup.cmo string_semigroup.cmo";
           ocamlc.byte;

           module = "";
           flags = "-instantiate -as-argument-for Monoid";
           program = "monoid_utils-Monoid_of_semigroup--String_semigroup.cmo";
           all_modules = "monoid_utils.cmo monoid_of_semigroup-String_semigroup.cmo";
           ocamlc.byte;

           module = "";
           flags = "-instantiate -as-argument-for Category";
           program = "category_of_monoid-List_monoid--Int_list_element.cmo";
           all_modules = "category_of_monoid.cmo list_monoid-Int_list_element.cmo";
           ocamlc.byte;

           module = "";
           flags = "-instantiate -as-argument-for Category";
           program = "category_of_monoid-Monoid_of_semigroup--String_semigroup.cmo";
           all_modules = "category_of_monoid.cmo monoid_of_semigroup-String_semigroup.cmo";
           ocamlc.byte;

           module = "";
           flags = "-instantiate";
           program = "chain-Category_of_monoid--List_monoid---Int_list_element.cmo";
           all_modules = "chain.cmo category_of_monoid-List_monoid--Int_list_element.cmo";
           ocamlc.byte;

           module = "";
           flags = "-instantiate";
           program = "chain-Category_of_monoid--Monoid_of_semigroup---String_semigroup.cmo";
           all_modules = "chain.cmo category_of_monoid-Monoid_of_semigroup--String_semigroup.cmo";
           ocamlc.byte;

           module = "";
           flags = "-instantiate";
           program = "import-Int_list_element-String_semigroup.cmo";
           all_modules = "import.cmo int_list_element.cmo string_semigroup.cmo";
           ocamlc.byte;

           module = "";
           flags = "-instantiate";
           program = "category_utils-Category_of_monoid--List_monoid---Int_list_element.cmo";
           all_modules = "category_utils.cmo category_of_monoid-List_monoid--Int_list_element.cmo";
           ocamlc.byte;

           module = "";
           flags = "-instantiate";
           program = "category_utils-Category_of_monoid--Monoid_of_semigroup---String_semigroup.cmo";
           all_modules = "category_utils.cmo category_of_monoid-Monoid_of_semigroup--String_semigroup.cmo";
           ocamlc.byte;

           flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute";
           module = "main.mli";
           ocamlc.byte;
           {
             flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute -i";
             module = "main.ml";
             compiler_output = "main.output";
             ocamlc.byte;

             compiler_reference = "main.reference";
             check-ocamlc.byte-output;
           }{
             module = "main.ml";
             ocamlc.byte;
             {
               program = "main.cmo main.cmi";
               output = "main-ocamlobjinfo.output";
               ocamlobjinfo;

               reference = "main-ocamlobjinfo.reference";
               check-program-output;
             }{
               module = "";
               flags = "-instantiate";
               program = "main-Int_list_element-String_semigroup.cmo";
               all_modules = "main.cmo int_list_element.cmo string_semigroup.cmo";
               ocamlc.byte;

               flags = "-w -misplaced-attribute";
               module = "test.ml";
               ocamlc.byte;

               flags = "";
               program = "${test_build_directory}/test.bc";
               module = "";
               all_modules = "\
                  string_semigroup.cmo \
                  monoid_of_semigroup.cmo \
                  monoid_of_semigroup-String_semigroup.cmo \
                  monoid_utils.cmo \
                  monoid_utils-Monoid_of_semigroup--String_semigroup.cmo \
                  int_list_element.cmo \
                  list_monoid.cmo \
                  list_monoid-Int_list_element.cmo \
                  category_of_monoid.cmo \
                  category_of_monoid-List_monoid--Int_list_element.cmo \
                  category_of_monoid-Monoid_of_semigroup--String_semigroup.cmo \
                  chain.cmo \
                  chain-Category_of_monoid--List_monoid---Int_list_element.cmo \
                  chain-Category_of_monoid--Monoid_of_semigroup---String_semigroup.cmo \
                  category_utils.cmo \
                  category_utils-Category_of_monoid--List_monoid---Int_list_element.cmo \
                  category_utils-Category_of_monoid--Monoid_of_semigroup---String_semigroup.cmo \
                  import.cmo \
                  import-Int_list_element-String_semigroup.cmo \
                  main.cmo \
                  main-Int_list_element-String_semigroup.cmo \
                  test.cmo \
               ";
               ocamlc.byte;

               output = "test.output";
               exit_status = "0";
               run;

               reference = "test.reference";
               check-program-output;
             }
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
     module = "semigroup.mli";
     ocamlopt.byte;

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
       flags = "";
       module = "bad_ref_indirect.ml";
       compiler_output = "bad_ref_indirect.output";
       ocamlopt_byte_exit_status = "2";
       ocamlopt.byte;

       compiler_reference = "bad_ref_indirect.reference";
       check-ocamlopt.byte-output;
     }{
       (* Linking an uninstantiated parameterised module is weird but should be harmless
          (it's just a module that exports a single functor). We should probably warn in
          this case, since it's probably not what the user meant to do. *)

       program = "${test_build_directory}/monoid_utils_as_program.exe";
       module = "";
       all_modules = "monoid_utils.cmx ";
       ocamlopt.byte;

       output = "monoid_utils_as_program.output";
       run;

       reference = "monoid_utils_as_program.reference";
       check-program-output;
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

         flags = "-as-argument-for Semigroup";
         module = "string_semigroup.mli";
         ocamlopt.byte;

         module = "string_semigroup.ml";
         ocamlopt.byte;

         module = "";
         flags = "-instantiate";
         program = "monoid_of_semigroup-String_semigroup.cmx";
         all_modules = "monoid_of_semigroup.cmx string_semigroup.cmx";
         ocamlopt.byte;

         flags = "-as-argument-for List_element";
         module = "int_list_element.mli int_list_element.ml";
         ocamlopt.byte;

         (* CR lmaurer: I'm indenting everything after these [bad_instantiate_*] tests
            under protest. See PR 2693 for a better way. *)

         {
           module = "";
           flags = "-instantiate";
           program = "int_list_element-String_semigroup.cmx";
           all_modules = "int_list_element.cmx string_semigroup.cmx";
           compiler_output = "bad_instantiate_not_parameterised_cmx.output";
           ocamlopt_byte_exit_status = "2";
           ocamlopt.byte;

           compiler_reference = "bad_instantiate_not_parameterised_cmx.reference";
           check-ocamlopt.byte-output;
         }{
           module = "";
           flags = "-instantiate -as-argument-for Monoid";
           program = "list_monoid-Int_list_element.cmx";
           all_modules = "list_monoid.cmx int_list_element.cmx";
           ocamlopt.byte;

           module = "";
           flags = "-instantiate -as-argument-for Monoid";
           program = "monoid_of_semigroup-String_semigroup.cmx";
           all_modules = "monoid_of_semigroup.cmx string_semigroup.cmx";
           ocamlopt.byte;

           module = "";
           flags = "-instantiate -as-argument-for Monoid";
           program = "monoid_utils-Monoid_of_semigroup--String_semigroup.cmx";
           all_modules = "monoid_utils.cmx monoid_of_semigroup-String_semigroup.cmx";
           ocamlopt.byte;

           module = "";
           flags = "-instantiate -as-argument-for Category";
           program = "category_of_monoid-List_monoid--Int_list_element.cmx";
           all_modules = "category_of_monoid.cmx list_monoid-Int_list_element.cmx";
           ocamlopt.byte;

           module = "";
           flags = "-instantiate -as-argument-for Category";
           program = "category_of_monoid-Monoid_of_semigroup--String_semigroup.cmx";
           all_modules = "category_of_monoid.cmx monoid_of_semigroup-String_semigroup.cmx";
           ocamlopt.byte;

           module = "";
           flags = "-instantiate";
           program = "chain-Category_of_monoid--List_monoid---Int_list_element.cmx";
           all_modules = "chain.cmx category_of_monoid-List_monoid--Int_list_element.cmx";
           ocamlopt.byte;

           module = "";
           flags = "-instantiate";
           program = "chain-Category_of_monoid--Monoid_of_semigroup---String_semigroup.cmx";
           all_modules = "chain.cmx category_of_monoid-Monoid_of_semigroup--String_semigroup.cmx";
           ocamlopt.byte;

           module = "";
           flags = "-instantiate";
           program = "import-Int_list_element-String_semigroup.cmx";
           all_modules = "import.cmx int_list_element.cmx string_semigroup.cmx";
           ocamlopt.byte;

           module = "";
           flags = "-instantiate";
           program = "category_utils-Category_of_monoid--List_monoid---Int_list_element.cmx";
           all_modules = "category_utils.cmx category_of_monoid-List_monoid--Int_list_element.cmx";
           ocamlopt.byte;

           module = "";
           flags = "-instantiate";
           program = "category_utils-Category_of_monoid--Monoid_of_semigroup---String_semigroup.cmx";
           all_modules = "category_utils.cmx category_of_monoid-Monoid_of_semigroup--String_semigroup.cmx";
           ocamlopt.byte;

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

             module = "";
             flags = "-instantiate";
             program = "main-Int_list_element-String_semigroup.cmx";
             all_modules = "main.cmx int_list_element.cmx string_semigroup.cmx";
             ocamlopt.byte;

             flags = "-w -misplaced-attribute";
             module = "test.ml";
             ocamlopt.byte;

             flags = "";
             program = "${test_build_directory}/test.exe";
             module = "";
             all_modules = "\
                string_semigroup.cmx \
                monoid_of_semigroup.cmx \
                monoid_of_semigroup-String_semigroup.cmx \
                monoid_utils.cmx \
                monoid_utils-Monoid_of_semigroup--String_semigroup.cmx \
                int_list_element.cmx \
                list_monoid.cmx \
                list_monoid-Int_list_element.cmx \
                category_of_monoid.cmx \
                category_of_monoid-List_monoid--Int_list_element.cmx \
                category_of_monoid-Monoid_of_semigroup--String_semigroup.cmx \
                chain.cmx \
                chain-Category_of_monoid--List_monoid---Int_list_element.cmx \
                chain-Category_of_monoid--Monoid_of_semigroup---String_semigroup.cmx \
                category_utils.cmx \
                category_utils-Category_of_monoid--List_monoid---Int_list_element.cmx \
                category_utils-Category_of_monoid--Monoid_of_semigroup---String_semigroup.cmx \
                import.cmx \
                import-Int_list_element-String_semigroup.cmx \
                main.cmx \
                main-Int_list_element-String_semigroup.cmx \
                test.cmx \
             ";
             ocamlopt.byte;

             output = "test.output";
             exit_status = "0";
             run;

             reference = "test.reference";
             check-program-output;
           }
         }
       }
     }
   }
 }
*)

module M =
  Main(List_element)(Int_list_element)(Semigroup)(String_semigroup)
  [@jane.non_erasable.instances]

let ints =
  M.append3_lists
    [4; 8]
    (M.concat_lists [[15]; []])
    (M.concat_chain_lists [[]; [16; -1]; []; [23; 42]])

let greeting =
  match
    M.concat_string_options [Some "Hello "; None; Some "world"; Some "!\n"; None]
  with
  | Some greeting -> greeting
  | None -> assert false

let ints_line =
  List.map (fun i -> if i > 0 then Some (Format.sprintf " %i" i) else None) ints
  |> M.concat_semi

let s = M.append3_semi (Some greeting) ints_line None |> Option.get

let () = print_endline s
