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
"

<TEST TREE HERE>
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
