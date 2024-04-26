let () =
  let enabled_if =
      {|(enabled_if (= %{context_name} "main"))|}
  in
  let buf = Buffer.create 1000 in
  let print_test ?(extra_flags="-zero-alloc-check default") deps =
    let subst = function
      | "enabled_if" -> enabled_if
      | "deps" -> deps
      | "extra_flags" -> extra_flags
      | _ -> "assert false"
    in
    Buffer.clear buf;
    Buffer.add_substitute buf subst
    {|
(rule
 (alias   runtest)
 ${enabled_if}
 (deps ${deps})
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c ${extra_flags} -dcse -dcheckmach -dump-into-file -O3 -warn-error +a)))
|};
    Buffer.output_buffer Out_channel.stdout buf
  in
  let print_test_expected_output ?(extra_flags="-zero-alloc-check default") ~cutoff ~extra_dep ~exit_code name =
    let ml_deps =
      let s =
        match extra_dep with
        | None -> ""
        | Some s -> s^" "
      in
      Printf.sprintf {|(:ml %s%s.ml)|} s name
    in
    let subst = function
      | "enabled_if" -> enabled_if
      | "name" -> name
      | "ml_deps" -> ml_deps
      | "exit_code" -> string_of_int exit_code
      | "cutoff" -> string_of_int cutoff
      | "extra_flags" -> extra_flags
      | _ -> assert false
    in
    Buffer.clear buf;
    Buffer.add_substitute buf subst
    {|
(rule
 ${enabled_if}
 (targets ${name}.output.corrected)
 (deps ${ml_deps} filter.sh)
 (action
   (with-outputs-to ${name}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes ${exit_code}
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          ${extra_flags} -checkmach-details-cutoff ${cutoff} -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 ${enabled_if}
 (deps ${name}.output ${name}.output.corrected)
 (action (diff ${name}.output ${name}.output.corrected)))
|};
    Buffer.output_buffer Out_channel.stdout buf
  in
  let default_cutoff = 20 in
  print_test "s.ml t.ml";
  print_test "t5.ml test_assume.ml";
  print_test "test_match_on_mutable_state.ml";
  print_test "test_flambda.ml";

  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail1";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail2";
  print_test_expected_output ~cutoff:0 ~extra_dep:(Some "t3.ml")
    ~exit_code:2 "fail3";
  print_test_expected_output ~cutoff:0 ~extra_dep:(Some "t4.ml")
    ~exit_code:2 "fail4";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail5";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail6";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail7";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:0 "fail8";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail9";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail10";
  print_test_expected_output ~cutoff:default_cutoff  ~extra_dep:None ~exit_code:2 "fail12";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail13";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail14";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail15";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail16";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail17";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail18";
  (* test printing detailed error message only in flambda because the exact output depends
     on optimization level. *)
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:(Some "dep19.ml") ~exit_code:2 "fail19";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "fail20";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "fail21";

  print_test_expected_output ~cutoff:0 ~extra_dep:None
    ~exit_code:2 "test_attribute_error_duplicate";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None
    ~exit_code:2 "test_attr_unused";
  (* Checks that the warning is printed and compilation is successful. *)
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None
    ~exit_code:0 "t6";
  (* Check that entry function and functors are ignored with  [@@@zero_alloc all] *)
  print_test "t7.ml";
  (* Check that compiler generated stubs are ignored with [@@@zero_alloc all] *)
  print_test "test_stub_dep.ml test_stub.ml";
  (* generates an indirect call. *)
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "t1";
  (* deleting dead functions works *)
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:(Some "test_warning199.mli") ~exit_code:0 "test_warning199";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:0 "test_never_returns_normally";
  print_test_expected_output ~extra_flags:"-zero-alloc-check opt" ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "fail22";
  print_test_expected_output ~extra_flags:"-zero-alloc-check opt" ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "fail23";
  print_test ~extra_flags:"-zero-alloc-check opt" "test_zero_alloc_opt1.ml";
  print_test ~extra_flags:"-zero-alloc-check opt" "test_zero_alloc_opt2.ml";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "test_assume_fail";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "test_assume_on_call";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "test_misplaced_assume";
  print_test_expected_output ~extra_flags:"-zero-alloc-check all" ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "test_attr_check";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "test_attr_check_all";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "test_attr_check_opt";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:0 "test_attr_check_none";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "fail24";
  print_test ~extra_flags:"-zero-alloc-check default -function-layout topological"  "test_raise_message.ml";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_flags:"-zero-alloc-check default -disable-precise-checkmach -function-layout source"
    ~extra_dep:None ~exit_code:2 "fail25";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_flags:"-zero-alloc-check default -disable-checkmach -function-layout source"
    ~extra_dep:None ~exit_code:2 "fail26";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_flags:"-zero-alloc-check default"
    ~extra_dep:None ~exit_code:2 "test_all_opt";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_flags:"-zero-alloc-check all"
    ~extra_dep:None ~exit_code:2 "test_all_opt2";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_flags:"-zero-alloc-check opt"
    ~extra_dep:None ~exit_code:2 "test_all_opt3";
  ()
