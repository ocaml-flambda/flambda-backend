
(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test1_runner.exe test1.cmx.dump)
 (deps test1.mli test1.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dsel -dlinear -dlive -regalloc cfg -extension simd -no-vectorize -o test1_runner.exe)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test1.output
   (run ./test1_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test1.expected test1.output)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test1.ml test1_vectorized.ml)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test1.mli test1_vectorized.mli)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test1_vectorized_runner.exe test1_vectorized.cmx.dump)
 (deps test1_vectorized.mli test1_vectorized.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dsel -dlinear -dlive -regalloc cfg -extension simd -vectorize -o test1_vectorized_runner.exe)))

(rule
 (enabled_if (= %{context_name} "main"))
 (target test1_vectorized.cmx.dump.output)
 (deps ./filter.sh test1_vectorized.cmx.dump)
 (action
  (with-outputs-to
   %{target}
   (run %{deps}))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test1_vectorized.cmx.dump.expected test1_vectorized.cmx.dump.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test1_vectorized.output
   (run ./test1_vectorized_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test1.expected test1_vectorized.expected)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test1_vectorized.expected test1_vectorized.output)))
