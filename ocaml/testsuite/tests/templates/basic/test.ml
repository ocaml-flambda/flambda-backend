(* If this is [test.ml], instead change [test.in.ml] and/or [gen_test.ml] and
   re-run [gen_test.ml]. *)

(* TEST

readonly_files = "\
  bad_arg_impl.ml bad_arg_impl.reference \
  bad_arg_intf.mli bad_arg_intf.reference \
  bad_instance_arg_name_not_found.ml bad_instance_arg_name_not_found.reference \
  bad_instance_arg_value_not_arg.ml bad_instance_arg_value_not_arg.reference \
  bad_instance_arg_value_not_found.ml bad_instance_arg_value_not_found.reference \
  bad_instance_arg_value_wrong_type.ml bad_instance_arg_value_wrong_type.reference \
  bad_ref_direct.ml bad_ref_direct.reference \
  bad_ref_direct_imported.ml bad_ref_direct_imported.reference \
  bad_ref_indirect.ml bad_ref_indirect.reference \
  category.ml category.mli \
  category_of_monoid.ml category_of_monoid.mli \
  category_utils.ml category_utils.mli \
  chain.ml chain.mli \
  import.ml \
  list_element.mli \
  list_monoid.ml list_monoid.mli \
  main.ml main.mli main.reference \
  monoid.mli \
  monoid_of_semigroup.ml monoid_of_semigroup.mli \
  monoid_utils.ml monoid_utils.mli monoid_utils_as_program.reference \
  semigroup.mli \
  string_monoid.ml string_monoid.mli \
  test_direct_access.ml test_direct_access.reference \
"

* setup-ocamlc.byte-build-env
** ocamlc.byte
flags = "-as-parameter"
module = "monoid.mli"
*** ocamlc.byte
flags = ""
module = "bad_ref_direct.ml"
compiler_output = "bad_ref_direct.output"
ocamlc_byte_exit_status = "2"
**** check-ocamlc.byte-output
compiler_reference = "bad_ref_direct.reference"
*** ocamlc.byte
flags = "-as-argument-for Monoid"
module = "bad_arg_impl.ml"
compiler_output = "bad_arg_impl.output"
ocamlc_byte_exit_status = "2"
**** check-ocamlc.byte-output
compiler_reference = "bad_arg_impl.reference"
*** ocamlc.byte
flags = "-as-argument-for Monoid"
module = "bad_arg_intf.mli"
compiler_output = "bad_arg_intf.output"
ocamlc_byte_exit_status = "2"
**** check-ocamlc.byte-output
compiler_reference = "bad_arg_intf.reference"
*** copy
src = "string_monoid.ml"
dst = "string_monoid_no_mli.ml"
**** ocamlc.byte
flags = "-as-argument-for Monoid"
module = "string_monoid_no_mli.ml string_monoid.mli string_monoid.ml"
***** ocamlc.byte
flags = ""
module = "test_direct_access.ml"
****** ocamlc.byte
flags = ""
program = "${test_build_directory}/test_direct_access.bc"
module = ""
all_modules = "\
   string_monoid.cmo \
   string_monoid_no_mli.cmo \
   test_direct_access.cmo \
"
******* run
output = "test_direct_access.output"
exit_status = "0"
******** check-program-output
reference = "test_direct_access.reference"
*** ocamlc.byte
module = "semigroup.mli"
**** ocamlc.byte
module = "category.mli"
***** ocamlc.byte
flags = "-parameter Semigroup -as-argument-for Monoid"
module = "monoid_of_semigroup.mli"
****** ocamlc.byte
module = "monoid_of_semigroup.ml"
******* ocamlc.byte
flags = "-as-parameter"
module = "list_element.mli"
******** ocamlc.byte
flags = "-parameter List_element -as-argument-for Monoid"
module = "list_monoid.mli list_monoid.ml"
********* ocamlc.byte
flags = "-parameter Monoid"
module = "monoid_utils.mli monoid_utils.ml"
********** ocamlc.byte
flags = ""
module = "bad_ref_indirect.ml"
compiler_output = "bad_ref_indirect.output"
ocamlc_byte_exit_status = "2"
*********** check-ocamlc.byte-output
compiler_reference = "bad_ref_indirect.reference"
********** ocamlc.byte
program = "${test_build_directory}/monoid_utils_as_program.bc"
module = ""
all_modules = "\
   monoid_utils.cmo \
"
*********** run
output = "monoid_utils_as_program.output"
exit_status = "2"
************ check-program-output
reference = "monoid_utils_as_program.reference"
********** ocamlc.byte
flags = "-parameter List_element"
module = "bad_instance_arg_name_not_found.ml"
compiler_output = "bad_instance_arg_name_not_found.output"
ocamlc_byte_exit_status = "2"
*********** check-ocamlc.byte-output
compiler_reference = "bad_instance_arg_name_not_found.reference"
********** ocamlc.byte
flags = "-parameter List_element"
module = "bad_instance_arg_value_not_arg.ml"
compiler_output = "bad_instance_arg_value_not_arg.output"
ocamlc_byte_exit_status = "2"
*********** check-ocamlc.byte-output
compiler_reference = "bad_instance_arg_value_not_arg.reference"
********** ocamlc.byte
flags = "-parameter List_element"
module = "bad_instance_arg_value_not_found.ml"
compiler_output = "bad_instance_arg_value_not_found.output"
ocamlc_byte_exit_status = "2"
*********** check-ocamlc.byte-output
compiler_reference = "bad_instance_arg_value_not_found.reference"
********** ocamlc.byte
flags = "-parameter Semigroup"
module = "bad_ref_direct_imported.ml"
compiler_output = "bad_ref_direct_imported.output"
ocamlc_byte_exit_status = "2"
*********** check-ocamlc.byte-output
compiler_reference = "bad_ref_direct_imported.reference"
********** ocamlc.byte
flags = "-parameter Category"
module = "chain.mli chain.ml"
*********** ocamlc.byte
flags = "-parameter Category"
module = "category_utils.mli category_utils.ml"
************ ocamlc.byte
flags = "-parameter Monoid -as-argument-for Category"
module = "category_of_monoid.mli category_of_monoid.ml"
************* ocamlc.byte
flags = "-parameter List_element"
module = "bad_instance_arg_value_wrong_type.ml"
compiler_output = "bad_instance_arg_value_wrong_type.output"
ocamlc_byte_exit_status = "2"
************** check-ocamlc.byte-output
compiler_reference = "bad_instance_arg_value_wrong_type.reference"
************* ocamlc.byte
flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute"
module = "import.ml"
************** ocamlc.byte
flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute"
module = "main.mli"
*************** ocamlc.byte
flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute -i"
module = "main.ml"
**************** check-ocamlc.byte-output
compiler_reference = "main.reference"
*************** ocamlc.byte
module = "main.ml"
**************** ocamlobjinfo
program = "main.cmo main.cmi"
***************** check-program-output
* setup-ocamlopt.byte-build-env
** ocamlopt.byte
flags = "-as-parameter"
module = "monoid.mli"
*** ocamlopt.byte
flags = ""
module = "bad_ref_direct.ml"
compiler_output = "bad_ref_direct.output"
ocamlopt_byte_exit_status = "2"
**** check-ocamlopt.byte-output
compiler_reference = "bad_ref_direct.reference"
*** ocamlopt.byte
flags = "-as-argument-for Monoid"
module = "bad_arg_impl.ml"
compiler_output = "bad_arg_impl.output"
ocamlopt_byte_exit_status = "2"
**** check-ocamlopt.byte-output
compiler_reference = "bad_arg_impl.reference"
*** ocamlopt.byte
flags = "-as-argument-for Monoid"
module = "bad_arg_intf.mli"
compiler_output = "bad_arg_intf.output"
ocamlopt_byte_exit_status = "2"
**** check-ocamlopt.byte-output
compiler_reference = "bad_arg_intf.reference"
*** copy
src = "string_monoid.ml"
dst = "string_monoid_no_mli.ml"
**** ocamlopt.byte
flags = "-as-argument-for Monoid"
module = "string_monoid_no_mli.ml string_monoid.mli string_monoid.ml"
***** ocamlopt.byte
flags = ""
module = "test_direct_access.ml"
****** ocamlopt.byte
flags = ""
program = "${test_build_directory}/test_direct_access.exe"
module = ""
all_modules = "\
   string_monoid.cmx \
   string_monoid_no_mli.cmx \
   test_direct_access.cmx \
"
******* run
output = "test_direct_access.output"
exit_status = "0"
******** check-program-output
reference = "test_direct_access.reference"
*** ocamlopt.byte
module = "semigroup.mli"
**** ocamlopt.byte
module = "category.mli"
***** ocamlopt.byte
flags = "-parameter Semigroup -as-argument-for Monoid"
module = "monoid_of_semigroup.mli"
****** ocamlopt.byte
module = "monoid_of_semigroup.ml"
******* ocamlopt.byte
flags = "-as-parameter"
module = "list_element.mli"
******** ocamlopt.byte
flags = "-parameter List_element -as-argument-for Monoid"
module = "list_monoid.mli list_monoid.ml"
********* ocamlopt.byte
flags = "-parameter Monoid"
module = "monoid_utils.mli monoid_utils.ml"
********** ocamlopt.byte
flags = ""
module = "bad_ref_indirect.ml"
compiler_output = "bad_ref_indirect.output"
ocamlopt_byte_exit_status = "2"
*********** check-ocamlopt.byte-output
compiler_reference = "bad_ref_indirect.reference"
********** ocamlopt.byte
program = "${test_build_directory}/monoid_utils_as_program.exe"
module = ""
all_modules = "\
   monoid_utils.cmx \
"
*********** run
output = "monoid_utils_as_program.output"
exit_status = "2"
************ check-program-output
reference = "monoid_utils_as_program.reference"
********** ocamlopt.byte
flags = "-parameter List_element"
module = "bad_instance_arg_name_not_found.ml"
compiler_output = "bad_instance_arg_name_not_found.output"
ocamlopt_byte_exit_status = "2"
*********** check-ocamlopt.byte-output
compiler_reference = "bad_instance_arg_name_not_found.reference"
********** ocamlopt.byte
flags = "-parameter List_element"
module = "bad_instance_arg_value_not_arg.ml"
compiler_output = "bad_instance_arg_value_not_arg.output"
ocamlopt_byte_exit_status = "2"
*********** check-ocamlopt.byte-output
compiler_reference = "bad_instance_arg_value_not_arg.reference"
********** ocamlopt.byte
flags = "-parameter List_element"
module = "bad_instance_arg_value_not_found.ml"
compiler_output = "bad_instance_arg_value_not_found.output"
ocamlopt_byte_exit_status = "2"
*********** check-ocamlopt.byte-output
compiler_reference = "bad_instance_arg_value_not_found.reference"
********** ocamlopt.byte
flags = "-parameter Semigroup"
module = "bad_ref_direct_imported.ml"
compiler_output = "bad_ref_direct_imported.output"
ocamlopt_byte_exit_status = "2"
*********** check-ocamlopt.byte-output
compiler_reference = "bad_ref_direct_imported.reference"
********** ocamlopt.byte
flags = "-parameter Category"
module = "chain.mli chain.ml"
*********** ocamlopt.byte
flags = "-parameter Category"
module = "category_utils.mli category_utils.ml"
************ ocamlopt.byte
flags = "-parameter Monoid -as-argument-for Category"
module = "category_of_monoid.mli category_of_monoid.ml"
************* ocamlopt.byte
flags = "-parameter List_element"
module = "bad_instance_arg_value_wrong_type.ml"
compiler_output = "bad_instance_arg_value_wrong_type.output"
ocamlopt_byte_exit_status = "2"
************** check-ocamlopt.byte-output
compiler_reference = "bad_instance_arg_value_wrong_type.reference"
************* ocamlopt.byte
flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute"
module = "import.ml"
************** ocamlopt.byte
flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute"
module = "main.mli"
*************** ocamlopt.byte
flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute -i"
module = "main.ml"
**************** check-ocamlopt.byte-output
compiler_reference = "main.reference"
*************** ocamlopt.byte
module = "main.ml"
*)
