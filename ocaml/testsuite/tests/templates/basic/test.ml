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
  int_list_element.ml int_list_element.mli \
  list_element.mli \
  list_monoid.ml list_monoid.mli \
  main.ml main.mli main.reference main-ocamlobjinfo.reference \
  monoid.mli \
  monoid_of_semigroup.ml monoid_of_semigroup.mli \
  monoid_utils.ml monoid_utils.mli \
  run.ml \
  semigroup.mli \
  string_monoid.ml string_monoid.mli \
  string_semigroup.ml string_semigroup.mli \
  test_direct_access.ml test_direct_access.reference \
"

* setup-ocamlc.byte-build-env
** ocamlc.byte
flags = "-g -as-parameter"
module = "monoid.mli"
*** ocamlc.byte
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
flags = "-g -as-argument-for Monoid"
module = "bad_arg_intf.mli"
compiler_output = "bad_arg_intf.output"
ocamlc_byte_exit_status = "2"
**** check-ocamlc.byte-output
compiler_reference = "bad_arg_intf.reference"
*** copy
src = "string_monoid.ml"
dst = "string_monoid_no_mli.ml"
**** ocamlc.byte
flags = "-g -as-argument-for Monoid"
module = "string_monoid_no_mli.ml string_monoid.mli string_monoid.ml"
***** ocamlc.byte
flags = "-g"
module = "test_direct_access.ml"
****** ocamlc.byte
flags = "-g"
program = "${test_build_directory}/test_direct_access.exe"
module = ""
all_modules = "string_monoid.cmo string_monoid_no_mli.cmo test_direct_access.cmo"
******* run
output = "test_direct_access.output"
******** check-program-output
reference = "test_direct_access.reference"
*** ocamlc.byte
module = "semigroup.mli"
**** ocamlc.byte
module = "category.mli"
***** ocamlc.byte
flags = "-g -parameter Semigroup -as-argument-for Monoid"
module = "monoid_of_semigroup.mli"
****** ocamlc.byte
(* Invoke the compiler separately on .mli and .ml just this once to make sure
   things work this way as well *)
module = "monoid_of_semigroup.ml"
******* ocamlc.byte
flags = "-g -as-parameter"
module = "list_element.mli"
******** ocamlc.byte
flags = "-g -parameter List_element -as-argument-for Monoid"
module = "list_monoid.mli list_monoid.ml"
********* ocamlc.byte
flags = "-g -parameter Monoid"
module = "monoid_utils.mli monoid_utils.ml"
********** ocamlc.byte
flags = "-g"
module = "bad_ref_indirect.ml"
compiler_output = "bad_ref_indirect.output"
ocamlc_byte_exit_status = "2"
*********** check-ocamlc.byte-output
compiler_reference = "bad_ref_indirect.reference"
********** ocamlc.byte
flags = "-g -parameter List_element"
module = "bad_instance_arg_name_not_found.ml"
compiler_output = "bad_instance_arg_name_not_found.output"
ocamlc_byte_exit_status = "2"
*********** check-ocamlc.byte-output
compiler_reference = "bad_instance_arg_name_not_found.reference"
********** ocamlc.byte
flags = "-g -parameter List_element"
module = "bad_instance_arg_value_not_arg.ml"
compiler_output = "bad_instance_arg_value_not_arg.output"
ocamlc_byte_exit_status = "2"
*********** check-ocamlc.byte-output
compiler_reference = "bad_instance_arg_value_not_arg.reference"
********** ocamlc.byte
flags = "-g -parameter List_element"
module = "bad_instance_arg_value_not_found.ml"
compiler_output = "bad_instance_arg_value_not_found.output"
ocamlc_byte_exit_status = "2"
*********** check-ocamlc.byte-output
compiler_reference = "bad_instance_arg_value_not_found.reference"
********** ocamlc.byte
flags = "-g -parameter Semigroup"
module = "bad_ref_direct_imported.ml"
compiler_output = "bad_ref_direct_imported.output"
ocamlc_byte_exit_status = "2"
*********** check-ocamlc.byte-output
compiler_reference = "bad_ref_direct_imported.reference"
********** ocamlc.byte
flags = "-g -parameter Category"
module = "chain.mli chain.ml"
*********** ocamlc.byte
flags = "-g -parameter Category"
module = "category_utils.mli category_utils.ml"
************ ocamlc.byte
flags = "-g -parameter Monoid -as-argument-for Category"
module = "category_of_monoid.mli category_of_monoid.ml"
************* ocamlc.byte
flags = "-g -parameter List_element"
module = "bad_instance_arg_value_wrong_type.ml"
compiler_output = "bad_instance_arg_value_wrong_type.output"
ocamlc_byte_exit_status = "2"
************** check-ocamlc.byte-output
compiler_reference = "bad_instance_arg_value_wrong_type.reference"
************* ocamlc.byte
flags = "-g -parameter Semigroup -parameter List_element -w -misplaced-attribute"
module = "import.ml"
************** ocamlc.byte
flags = "-g -as-argument-for Semigroup"
module = "string_semigroup.mli"
*************** ocamlc.byte
module = "string_semigroup.ml"
**************** ocamlc.byte
module = ""
flags = "-g -instantiate"
program = "monoid_of_semigroup-String_semigroup.cmo"
all_modules = "monoid_of_semigroup.cmo string_semigroup.cmo"
***************** ocamlc.byte
flags = "-g -parameter Semigroup -parameter List_element -w -misplaced-attribute"
module = "main.mli"
****************** ocamlc.byte
flags += " -i"
module = "main.ml"
compiler_output = "main.output"
******************* check-ocamlc.byte-output
compiler_reference = "main.reference"
****************** ocamlc.byte
module = "main.ml"
******************* ocamlobjinfo
program = "main.cmo main.cmi"
output = "main-ocamlobjinfo.output"
******************** check-program-output
reference = "main-ocamlobjinfo.reference"
******************* ocamlc.byte
flags = "-g -as-argument-for List_element"
module = "int_list_element.mli int_list_element.ml"
******************** ocamlc.byte
module = ""
flags = "-instantiate -as-argument-for Monoid"
program = "list_monoid-Int_list_element.cmo"
all_modules = "list_monoid.cmo int_list_element.cmo"
********************* ocamlc.byte
module = ""
flags = "-instantiate -as-argument-for Monoid"
program = "monoid_of_semigroup-String_semigroup.cmo"
all_modules = "monoid_of_semigroup.cmo string_semigroup.cmo"
********************** ocamlc.byte
module = ""
flags = "-instantiate -as-argument-for Monoid"
program = "monoid_utils-Monoid_of_semigroup--String_semigroup.cmo"
all_modules = "monoid_utils.cmo monoid_of_semigroup-String_semigroup.cmo"
*********************** ocamlc.byte
module = ""
flags = "-instantiate -as-argument-for Category"
program = "category_of_monoid-List_monoid--Int_list_element.cmo"
all_modules = "category_of_monoid.cmo list_monoid-Int_list_element.cmo"
************************ ocamlc.byte
module = ""
flags = "-instantiate -as-argument-for Category"
program = "category_of_monoid-Monoid_of_semigroup--String_semigroup.cmo"
all_modules = "category_of_monoid.cmo monoid_of_semigroup-String_semigroup.cmo"
************************* ocamlc.byte
module = ""
flags = "-instantiate"
program = "chain-Category_of_monoid--List_monoid---Int_list_element.cmo"
all_modules = "chain.cmo category_of_monoid-List_monoid--Int_list_element.cmo"
************************** ocamlc.byte
module = ""
flags = "-instantiate"
program = "chain-Category_of_monoid--Monoid_of_semigroup---String_semigroup.cmo"
all_modules = "chain.cmo category_of_monoid-Monoid_of_semigroup--String_semigroup.cmo"
*************************** ocamlc.byte
module = ""
flags = "-instantiate"
program = "import-Int_list_element-String_semigroup.cmo"
all_modules = "import.cmo int_list_element.cmo string_semigroup.cmo"
**************************** ocamlc.byte
module = ""
flags = "-instantiate"
program = "category_utils-Category_of_monoid--List_monoid---Int_list_element.cmo"
all_modules = "category_utils.cmo category_of_monoid-List_monoid--Int_list_element.cmo"
***************************** ocamlc.byte
module = ""
flags = "-instantiate"
program = "category_utils-Category_of_monoid--Monoid_of_semigroup---String_semigroup.cmo"
all_modules = "category_utils.cmo category_of_monoid-Monoid_of_semigroup--String_semigroup.cmo"
****************************** ocamlc.byte
module = ""
flags = "-g -instantiate"
program = "main-Int_list_element-String_semigroup.cmo"
all_modules = "main.cmo int_list_element.cmo string_semigroup.cmo"
******************************* ocamlc.byte
flags = "-g -w -misplaced-attribute"
module = "test.ml"
******************************** ocamlc.byte
flags = "-g"
module = ""
program = "test.exe"
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
"
********************************* run
program = "${test_build_directory}/test.exe"
********************************** check-program-output
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
