let enabled_if_main = {|(enabled_if (= %{context_name} "main"))|}

let enabled_if_main_amd64 =
  {|(enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )|}

let flags =
  "-S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg \
   -extension simd -vectorize-max-block-size 1000"

let runner name = name ^ "_runner.exe"

let output name = name ^ ".output"

let expected name = name ^ ".expected"

let impl name = name ^ ".ml"

let intf name = name ^ ".mli"

let cmx_dump name = name ^ ".cmx.dump"

let vectorized name = name ^ "_vectorized"

let buf = Buffer.create 1000

let rule ~subst template =
  Buffer.add_substitute buf subst template;
  Buffer.output_buffer Out_channel.stdout buf;
  Buffer.clear buf

let compile ~enabled_if ~extra_flags name =
  let subst = function
    | "enabled_if" -> enabled_if
    | "flags" -> flags
    | "extra_flags" -> extra_flags
    | "runner" -> runner name
    | "deps" -> String.concat " " [intf name; impl name]
    | "cmx_dump" -> cmx_dump name
    | _ -> assert false
  in
  rule ~subst
    {|
(rule
 (alias   runtest)
 ${enabled_if}
 (targets ${runner} ${cmx_dump})
 (deps ${deps})
 (action (run %{bin:ocamlopt.opt} %{deps} ${flags} ${extra_flags} -o ${runner})))
|}

let run ~enabled_if name =
  let subst = function
    | "enabled_if" -> enabled_if
    | "runner" -> runner name
    | "output" -> output name
    | _ -> assert false
  in
  rule ~subst
    {|
(rule
 (alias   runtest)
 ${enabled_if}
 (action
  (with-outputs-to
   ${output}
   (run ./${runner}))))
|}

let diff_output ~enabled_if name =
  let subst = function
    | "enabled_if" -> enabled_if
    | "output" -> output name
    | "expected" -> expected name
    | _ -> assert false
  in
  rule ~subst
    {|
(rule
 (alias runtest)
 ${enabled_if}
 (action
   (diff ${expected} ${output})))
|}

let copy_file ~enabled_if name new_name =
  let subst = function
    | "enabled_if" -> enabled_if
    | "source" -> name
    | "target" -> new_name
    | _ -> assert false
  in
  rule ~subst
    {|
(rule
 (alias runtest)
 ${enabled_if}
 (action
  (copy ${source} ${target})))
|}

let filter_dump ~enabled_if ~exit_code name =
  let subst = function
    | "enabled_if" -> enabled_if
    | "exit_code" -> string_of_int exit_code
    | "dump" -> name |> cmx_dump
    | "filtered" -> name |> cmx_dump |> output
    | _ -> assert false
  in
  rule ~subst
    {|
(rule
 ${enabled_if}
 (target ${filtered})
 (deps ./filter.sh ${dump})
 (action
  (with-outputs-to
   %{target}
   (with-accepted-exit-codes ${exit_code}
    (run %{deps})))))
|}

let copy_source_to_vectorize ~enabled_if name =
  copy_file ~enabled_if (name |> impl) (name |> vectorized |> impl);
  copy_file ~enabled_if (name |> intf) (name |> vectorized |> intf)

let compile_no_vectorizer ~enabled_if name =
  compile ~enabled_if ~extra_flags:"-no-vectorize" name

let compile_with_vectorizer ~enabled_if name =
  compile ~enabled_if ~extra_flags:"-vectorize" (vectorized name)

let filter_vectorizer_dump ~enabled_if ~exit_code name =
  filter_dump ~enabled_if ~exit_code (name |> vectorized)

let diff_vectorizer_dump ~enabled_if name =
  diff_output ~enabled_if (name |> vectorized |> cmx_dump)

let run_no_vectorizer ~enabled_if name = run ~enabled_if name

let run_vectorized ~enabled_if name = run ~enabled_if (name |> vectorized)

let diff_output_no_vectorizer ~enabled_if name = diff_output ~enabled_if name

let diff_output_vectorized ~enabled_if name =
  diff_output ~enabled_if (name |> vectorized)

let copy_expected_output ~enabled_if name =
  copy_file ~enabled_if (name |> expected) (name |> vectorized |> expected)

let print_test ?(enabled_if = enabled_if_main) ?(filter_exit_code = 0) name =
  (* check expected test output is up to date *)
  compile_no_vectorizer ~enabled_if name;
  run_no_vectorizer ~enabled_if name;
  diff_output_no_vectorizer ~enabled_if name;
  (* vectorizer *)
  copy_source_to_vectorize ~enabled_if name;
  compile_with_vectorizer ~enabled_if name;
  filter_vectorizer_dump name ~exit_code:filter_exit_code
    ~enabled_if:enabled_if_main_amd64;
  diff_vectorizer_dump name ~enabled_if:enabled_if_main_amd64;
  run_vectorized ~enabled_if name;
  copy_expected_output ~enabled_if name;
  diff_output_vectorized ~enabled_if name;
  ()

let () =
  print_test "test1";
  print_test "test_arrays";
  print_test "test_int64_unboxed";
  print_test "test_float_unboxed";
  print_test "test_int64";
  print_test "test_float";
  print_test ~enabled_if:enabled_if_main_amd64 "test_float32_unboxed";
  print_test "test_int32_unboxed";
  print_test "test_spill_valx2";
  (* can't vectorize *)
  print_test ~filter_exit_code:1 "test_register_compatible";
  ()

let () =
  (* test that vectorizer is disabled in classic mode *)
  let enabled_if = enabled_if_main in
  let name = "test1" in
  let name' = name ^ "_classic" in
  copy_file ~enabled_if (name |> impl) (name' |> impl);
  copy_file ~enabled_if (name |> intf) (name' |> intf);
  compile ~enabled_if ~extra_flags:"-Oclassic -vectorize" name';
  filter_dump ~enabled_if ~exit_code:1 name';
  ()
