let () =
  let enabled_if flambda_only =
    if flambda_only then
      (* CR-soon: what we really want to say if dune knew about flambda2:
         (or %{ocaml-config:flambda} %{ocaml-config:flambda2}) *)
      {|(enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))|}
    else
      {|(enabled_if (= %{context_name} "main"))|}
  in
  let buf = Buffer.create 1000 in
  let print_test ?(extra_flags="-zero-alloc-check") ~flambda_only deps =
    let enabled_if = enabled_if flambda_only in
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
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c ${extra_flags} -dcse -dcheckmach -dump-into-file -O3)))
|};
    Buffer.output_buffer Out_channel.stdout buf
  in
  let print_test_expected_output ?(extra_flags="-zero-alloc-check") ~cutoff ~flambda_only ~extra_dep ~exit_code name =
    let enabled_if = enabled_if flambda_only in
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
  print_test ~flambda_only:false "s.ml t.ml";
  print_test ~flambda_only:false "t5.ml test_assume.ml";
  print_test ~flambda_only:true "test_flambda.ml";

  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail1";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail2";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:(Some "t3.ml")
    ~exit_code:2 "fail3";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:(Some "t4.ml")
    ~exit_code:2 "fail4";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail5";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail6";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail7";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail8";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail9";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail10";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail11";
  print_test_expected_output ~cutoff:default_cutoff ~flambda_only:true  ~extra_dep:None ~exit_code:2 "fail12";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail13";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail14";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail15";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail16";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail17";
  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail18";
  (* test printing detailed error message only in flambda because the exact output depends
     on optimization level. *)
  print_test_expected_output ~cutoff:default_cutoff ~flambda_only:true ~extra_dep:(Some "dep19.ml") ~exit_code:2 "fail19";
  print_test_expected_output ~cutoff:default_cutoff ~flambda_only:true ~extra_dep:None ~exit_code:2 "fail20";
  print_test_expected_output ~cutoff:default_cutoff ~flambda_only:true ~extra_dep:None ~exit_code:2 "fail21";

  print_test_expected_output ~cutoff:0 ~flambda_only:false ~extra_dep:None
    ~exit_code:2 "test_attribute_error_duplicate";
  (* Closure does not optimize the function away, so the unchecked attribute
     warning is only with flambda and flambda2. *)
  print_test_expected_output ~cutoff:default_cutoff ~flambda_only:true ~extra_dep:None
    ~exit_code:0 "test_attr_unused";
  (* Checks that the warning is printed and compilation is successful. *)
  print_test_expected_output ~cutoff:default_cutoff ~flambda_only:false ~extra_dep:None
    ~exit_code:0 "t6";
  (* Check that entry function and functors are ignored with  [@@@zero_alloc all] *)
  print_test ~flambda_only:false "t7.ml";
  (* Check that compiler generated stubs are ignored with [@@@zero_alloc all] *)
  print_test ~flambda_only:false "test_stub_dep.ml test_stub.ml";
  (* flambda2 generates an indirect call but we don't yet have a way to exclude it
     without excluding closure. *)
  print_test ~flambda_only:true "t1.ml";
  (* closure does not delete dead functions *)
  print_test_expected_output ~cutoff:default_cutoff ~flambda_only:true ~extra_dep:(Some "test_warning199.mli") ~exit_code:0 "test_warning199";
  print_test_expected_output ~cutoff:default_cutoff ~flambda_only:true ~extra_dep:None ~exit_code:2 "test_never_returns_normally";
  print_test_expected_output ~extra_flags:"-zero-alloc-check-opt" ~cutoff:default_cutoff ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail22";
  print_test_expected_output ~extra_flags:"-zero-alloc-check-opt" ~cutoff:default_cutoff ~flambda_only:true ~extra_dep:None ~exit_code:2 "fail23";
  print_test ~extra_flags:"-zero-alloc-check-opt" ~flambda_only:false "test_zero_alloc_opt1.ml";
  print_test ~extra_flags:"-zero-alloc-check-opt" ~flambda_only:false "test_zero_alloc_opt2.ml";
  ()
