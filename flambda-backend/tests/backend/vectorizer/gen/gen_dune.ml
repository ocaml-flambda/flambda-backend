let enabled_if = {|(enabled_if (= %{context_name} "main"))|}

let flags =
  "-S -O3 -g -dump-into-file -dcfg -dvectorize -dsel -dlinear -dlive -regalloc \
   cfg -extension simd"

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

let compile ~extra_flags name =
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

let run name =
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

let diff_output name =
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

let copy_file name new_name =
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

let filter_dump name =
  let subst = function
    | "enabled_if" -> enabled_if
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
   (run %{deps}))))
|}

let copy_source_to_vectorize name =
  copy_file (name |> impl) (name |> vectorized |> impl);
  copy_file (name |> intf) (name |> vectorized |> intf)

let compile_no_vectorizer name = compile ~extra_flags:"-no-vectorize" name

let compile_with_vectorizer name =
  compile ~extra_flags:"-vectorize" (vectorized name)

let filter_vectorizer_dump name = filter_dump (name |> vectorized)

let diff_vectorizer_dump name = diff_output (name |> vectorized |> cmx_dump)

let run_no_vectorizer name = run name

let run_vectorized name = run (name |> vectorized)

let diff_output_no_vectorizer name = diff_output name

let diff_output_vectorized name = diff_output (name |> vectorized)

let copy_expected_output name =
  copy_file (name |> expected) (name |> vectorized |> expected)

let print_test name =
  (* check expected test output is up to date *)
  compile_no_vectorizer name;
  run_no_vectorizer name;
  diff_output_no_vectorizer name;
  (* vectorizer *)
  copy_source_to_vectorize name;
  compile_with_vectorizer name;
  filter_vectorizer_dump name;
  diff_vectorizer_dump name;
  run_vectorized name;
  copy_expected_output name;
  diff_output_vectorized name;
  ()

let () = print_test "test1"
