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

<TEST TREE HERE>
*)
