let () =
  let enabled_if flambda_only =
    if flambda_only then
      (* CR-soon: what we really want to say if dune knew about flambda2:
         (or %{ocaml-config:flambda} %{ocaml-config:flambda2}) *)
      {|(enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))|}
    else
      {|(enabled_if (= %{context_name} "main"))|}
  in
  let print_test ~flambda_only ~deps =
    let enabled_if = enabled_if flambda_only in
    Printf.printf
    {|
(rule
 (alias   runtest)
 %s
 (deps %s)
 (action (run %%{bin:ocamlopt.opt} %%{deps} -g -c -zero-alloc-check -dcse -dcheckmach -dump-into-file -O3)))
|}
    enabled_if deps
  in
  let print_test_expected_output ~flambda_only ~extra_dep ~exit_code name =
    let enabled_if = enabled_if flambda_only in
    let ml_deps =
      let s =
        match extra_dep with
        | None -> ""
        | Some s -> s^" "
      in
      Printf.sprintf {|(:ml %s%s.ml)|} s name
    in
  Printf.printf
    {|
(rule
 %s
 (targets %s.output.corrected)
 (deps %s filter.sh)
 (action
   (with-outputs-to %s.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes %d
     (run %%{bin:ocamlopt.opt} %%{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 %s
 (deps %s.output %s.output.corrected)
 (action (diff %s.output %s.output.corrected)))
|}
  enabled_if name ml_deps name exit_code enabled_if name name name name
  in
  print_test ~flambda_only:false ~deps:"s.ml t.ml";
  print_test ~flambda_only:false ~deps:"t5.ml test_assume.ml";
  print_test ~flambda_only:true ~deps:"test_flambda.ml";

  print_test_expected_output ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail1";
  print_test_expected_output ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail2";
  print_test_expected_output ~flambda_only:false ~extra_dep:(Some "t3.ml")
    ~exit_code:2 "fail3";
  print_test_expected_output ~flambda_only:false ~extra_dep:(Some "t4.ml")
    ~exit_code:2 "fail4";
  print_test_expected_output ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail5";
  print_test_expected_output ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail6";
  print_test_expected_output ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail7";
  print_test_expected_output ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail8";
  print_test_expected_output ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail9";
  print_test_expected_output ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail10";
  print_test_expected_output ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail11";
  print_test_expected_output ~flambda_only:true  ~extra_dep:None ~exit_code:2 "fail12";
  print_test_expected_output ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail13";
  print_test_expected_output ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail14";
  print_test_expected_output ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail15";
  print_test_expected_output ~flambda_only:false ~extra_dep:None ~exit_code:2 "fail16";

  print_test_expected_output ~flambda_only:false ~extra_dep:None
    ~exit_code:2 "test_attribute_error_duplicate";
  (* Closure does not optimize the function away, so the unchecked attribute
     warning is only with flambda and flambda2. *)
  print_test_expected_output ~flambda_only:true ~extra_dep:None
    ~exit_code:0 "test_attr_unused";
  (* Checks that the warning is printed and compilation is successful. *)
  print_test_expected_output ~flambda_only:false ~extra_dep:None
    ~exit_code:0 "t6";
  (* Check that entry function and functors are ignored with  [@@@zero_alloc all] *)
  print_test ~flambda_only:false ~deps:"t7.ml"
