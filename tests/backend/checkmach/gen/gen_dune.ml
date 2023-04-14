let () =
  for i = 1 to 12 do
    let extra_dep =
      match i with
      | 3 | 4 -> true
      | _ -> false
    in
    let flambda_only =
      match i with
      | 12 -> true
      | _ -> false
    in
    let enabled_if =
      if flambda_only then
        {|(enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))|}
      else
        {|(enabled_if (= %{context_name} "main"))|}
    in
    let ml_deps =
      let s =
        (if extra_dep then Printf.sprintf "t%d.ml " i else "")
      in
      Printf.sprintf {|(:ml %sfail%d.ml)|} s i
    in
  Printf.printf
    {|
(rule
 %s
 (targets fail%d.output.corrected)
 (deps %s filter.sh)
 (action
   (with-outputs-to fail%d.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %%{bin:ocamlopt.opt} %%{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 %s
 (deps fail%d.output fail%d.output.corrected)
 (action (diff fail%d.output fail%d.output.corrected)))
|}
  enabled_if i ml_deps i enabled_if i i i i
  done
