
(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test1_runner.exe test1.cmx.dump)
 (deps test1.mli test1.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -no-vectorize -o test1_runner.exe)))

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
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -vectorize -o test1_vectorized_runner.exe)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (target test1_vectorized.cmx.dump.output)
 (deps ./filter.sh test1_vectorized.cmx.dump)
 (action
  (with-outputs-to
   %{target}
   (with-accepted-exit-codes 0
    (run %{deps})))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
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

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_arrays_runner.exe test_arrays.cmx.dump)
 (deps test_arrays.mli test_arrays.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -no-vectorize -o test_arrays_runner.exe)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_arrays.output
   (run ./test_arrays_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_arrays.expected test_arrays.output)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_arrays.ml test_arrays_vectorized.ml)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_arrays.mli test_arrays_vectorized.mli)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_arrays_vectorized_runner.exe test_arrays_vectorized.cmx.dump)
 (deps test_arrays_vectorized.mli test_arrays_vectorized.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -vectorize -o test_arrays_vectorized_runner.exe)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (target test_arrays_vectorized.cmx.dump.output)
 (deps ./filter.sh test_arrays_vectorized.cmx.dump)
 (action
  (with-outputs-to
   %{target}
   (with-accepted-exit-codes 0
    (run %{deps})))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
   (diff test_arrays_vectorized.cmx.dump.expected test_arrays_vectorized.cmx.dump.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_arrays_vectorized.output
   (run ./test_arrays_vectorized_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_arrays.expected test_arrays_vectorized.expected)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_arrays_vectorized.expected test_arrays_vectorized.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_int64_unboxed_runner.exe test_int64_unboxed.cmx.dump)
 (deps test_int64_unboxed.mli test_int64_unboxed.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -no-vectorize -o test_int64_unboxed_runner.exe)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_int64_unboxed.output
   (run ./test_int64_unboxed_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_int64_unboxed.expected test_int64_unboxed.output)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_int64_unboxed.ml test_int64_unboxed_vectorized.ml)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_int64_unboxed.mli test_int64_unboxed_vectorized.mli)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_int64_unboxed_vectorized_runner.exe test_int64_unboxed_vectorized.cmx.dump)
 (deps test_int64_unboxed_vectorized.mli test_int64_unboxed_vectorized.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -vectorize -o test_int64_unboxed_vectorized_runner.exe)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (target test_int64_unboxed_vectorized.cmx.dump.output)
 (deps ./filter.sh test_int64_unboxed_vectorized.cmx.dump)
 (action
  (with-outputs-to
   %{target}
   (with-accepted-exit-codes 0
    (run %{deps})))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
   (diff test_int64_unboxed_vectorized.cmx.dump.expected test_int64_unboxed_vectorized.cmx.dump.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_int64_unboxed_vectorized.output
   (run ./test_int64_unboxed_vectorized_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_int64_unboxed.expected test_int64_unboxed_vectorized.expected)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_int64_unboxed_vectorized.expected test_int64_unboxed_vectorized.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_float_unboxed_runner.exe test_float_unboxed.cmx.dump)
 (deps test_float_unboxed.mli test_float_unboxed.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -no-vectorize -o test_float_unboxed_runner.exe)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_float_unboxed.output
   (run ./test_float_unboxed_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_float_unboxed.expected test_float_unboxed.output)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_float_unboxed.ml test_float_unboxed_vectorized.ml)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_float_unboxed.mli test_float_unboxed_vectorized.mli)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_float_unboxed_vectorized_runner.exe test_float_unboxed_vectorized.cmx.dump)
 (deps test_float_unboxed_vectorized.mli test_float_unboxed_vectorized.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -vectorize -o test_float_unboxed_vectorized_runner.exe)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (target test_float_unboxed_vectorized.cmx.dump.output)
 (deps ./filter.sh test_float_unboxed_vectorized.cmx.dump)
 (action
  (with-outputs-to
   %{target}
   (with-accepted-exit-codes 0
    (run %{deps})))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
   (diff test_float_unboxed_vectorized.cmx.dump.expected test_float_unboxed_vectorized.cmx.dump.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_float_unboxed_vectorized.output
   (run ./test_float_unboxed_vectorized_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_float_unboxed.expected test_float_unboxed_vectorized.expected)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_float_unboxed_vectorized.expected test_float_unboxed_vectorized.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_int64_runner.exe test_int64.cmx.dump)
 (deps test_int64.mli test_int64.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -no-vectorize -o test_int64_runner.exe)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_int64.output
   (run ./test_int64_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_int64.expected test_int64.output)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_int64.ml test_int64_vectorized.ml)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_int64.mli test_int64_vectorized.mli)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_int64_vectorized_runner.exe test_int64_vectorized.cmx.dump)
 (deps test_int64_vectorized.mli test_int64_vectorized.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -vectorize -o test_int64_vectorized_runner.exe)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (target test_int64_vectorized.cmx.dump.output)
 (deps ./filter.sh test_int64_vectorized.cmx.dump)
 (action
  (with-outputs-to
   %{target}
   (with-accepted-exit-codes 0
    (run %{deps})))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
   (diff test_int64_vectorized.cmx.dump.expected test_int64_vectorized.cmx.dump.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_int64_vectorized.output
   (run ./test_int64_vectorized_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_int64.expected test_int64_vectorized.expected)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_int64_vectorized.expected test_int64_vectorized.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_float_runner.exe test_float.cmx.dump)
 (deps test_float.mli test_float.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -no-vectorize -o test_float_runner.exe)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_float.output
   (run ./test_float_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_float.expected test_float.output)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_float.ml test_float_vectorized.ml)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_float.mli test_float_vectorized.mli)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_float_vectorized_runner.exe test_float_vectorized.cmx.dump)
 (deps test_float_vectorized.mli test_float_vectorized.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -vectorize -o test_float_vectorized_runner.exe)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (target test_float_vectorized.cmx.dump.output)
 (deps ./filter.sh test_float_vectorized.cmx.dump)
 (action
  (with-outputs-to
   %{target}
   (with-accepted-exit-codes 0
    (run %{deps})))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
   (diff test_float_vectorized.cmx.dump.expected test_float_vectorized.cmx.dump.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_float_vectorized.output
   (run ./test_float_vectorized_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_float.expected test_float_vectorized.expected)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_float_vectorized.expected test_float_vectorized.output)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_float32_unboxed_runner.exe test_float32_unboxed.cmx.dump)
 (deps test_float32_unboxed.mli test_float32_unboxed.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -no-vectorize -o test_float32_unboxed_runner.exe)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
  (with-outputs-to
   test_float32_unboxed.output
   (run ./test_float32_unboxed_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
   (diff test_float32_unboxed.expected test_float32_unboxed.output)))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
  (copy test_float32_unboxed.ml test_float32_unboxed_vectorized.ml)))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
  (copy test_float32_unboxed.mli test_float32_unboxed_vectorized.mli)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_float32_unboxed_vectorized_runner.exe test_float32_unboxed_vectorized.cmx.dump)
 (deps test_float32_unboxed_vectorized.mli test_float32_unboxed_vectorized.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -vectorize -o test_float32_unboxed_vectorized_runner.exe)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (target test_float32_unboxed_vectorized.cmx.dump.output)
 (deps ./filter.sh test_float32_unboxed_vectorized.cmx.dump)
 (action
  (with-outputs-to
   %{target}
   (with-accepted-exit-codes 0
    (run %{deps})))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
   (diff test_float32_unboxed_vectorized.cmx.dump.expected test_float32_unboxed_vectorized.cmx.dump.output)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
  (with-outputs-to
   test_float32_unboxed_vectorized.output
   (run ./test_float32_unboxed_vectorized_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
  (copy test_float32_unboxed.expected test_float32_unboxed_vectorized.expected)))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
   (diff test_float32_unboxed_vectorized.expected test_float32_unboxed_vectorized.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_int32_unboxed_runner.exe test_int32_unboxed.cmx.dump)
 (deps test_int32_unboxed.mli test_int32_unboxed.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -no-vectorize -o test_int32_unboxed_runner.exe)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_int32_unboxed.output
   (run ./test_int32_unboxed_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_int32_unboxed.expected test_int32_unboxed.output)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_int32_unboxed.ml test_int32_unboxed_vectorized.ml)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_int32_unboxed.mli test_int32_unboxed_vectorized.mli)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_int32_unboxed_vectorized_runner.exe test_int32_unboxed_vectorized.cmx.dump)
 (deps test_int32_unboxed_vectorized.mli test_int32_unboxed_vectorized.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -vectorize -o test_int32_unboxed_vectorized_runner.exe)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (target test_int32_unboxed_vectorized.cmx.dump.output)
 (deps ./filter.sh test_int32_unboxed_vectorized.cmx.dump)
 (action
  (with-outputs-to
   %{target}
   (with-accepted-exit-codes 0
    (run %{deps})))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
   (diff test_int32_unboxed_vectorized.cmx.dump.expected test_int32_unboxed_vectorized.cmx.dump.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_int32_unboxed_vectorized.output
   (run ./test_int32_unboxed_vectorized_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_int32_unboxed.expected test_int32_unboxed_vectorized.expected)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_int32_unboxed_vectorized.expected test_int32_unboxed_vectorized.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_spill_valx2_runner.exe test_spill_valx2.cmx.dump)
 (deps test_spill_valx2.mli test_spill_valx2.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -no-vectorize -o test_spill_valx2_runner.exe)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_spill_valx2.output
   (run ./test_spill_valx2_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_spill_valx2.expected test_spill_valx2.output)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_spill_valx2.ml test_spill_valx2_vectorized.ml)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_spill_valx2.mli test_spill_valx2_vectorized.mli)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_spill_valx2_vectorized_runner.exe test_spill_valx2_vectorized.cmx.dump)
 (deps test_spill_valx2_vectorized.mli test_spill_valx2_vectorized.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -vectorize -o test_spill_valx2_vectorized_runner.exe)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (target test_spill_valx2_vectorized.cmx.dump.output)
 (deps ./filter.sh test_spill_valx2_vectorized.cmx.dump)
 (action
  (with-outputs-to
   %{target}
   (with-accepted-exit-codes 0
    (run %{deps})))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
   (diff test_spill_valx2_vectorized.cmx.dump.expected test_spill_valx2_vectorized.cmx.dump.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_spill_valx2_vectorized.output
   (run ./test_spill_valx2_vectorized_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_spill_valx2.expected test_spill_valx2_vectorized.expected)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_spill_valx2_vectorized.expected test_spill_valx2_vectorized.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_register_compatible_runner.exe test_register_compatible.cmx.dump)
 (deps test_register_compatible.mli test_register_compatible.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -no-vectorize -o test_register_compatible_runner.exe)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_register_compatible.output
   (run ./test_register_compatible_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_register_compatible.expected test_register_compatible.output)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_register_compatible.ml test_register_compatible_vectorized.ml)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_register_compatible.mli test_register_compatible_vectorized.mli)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test_register_compatible_vectorized_runner.exe test_register_compatible_vectorized.cmx.dump)
 (deps test_register_compatible_vectorized.mli test_register_compatible_vectorized.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -vectorize -o test_register_compatible_vectorized_runner.exe)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (target test_register_compatible_vectorized.cmx.dump.output)
 (deps ./filter.sh test_register_compatible_vectorized.cmx.dump)
 (action
  (with-outputs-to
   %{target}
   (with-accepted-exit-codes 1
    (run %{deps})))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action
   (diff test_register_compatible_vectorized.cmx.dump.expected test_register_compatible_vectorized.cmx.dump.output)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_register_compatible_vectorized.output
   (run ./test_register_compatible_vectorized_runner.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_register_compatible.expected test_register_compatible_vectorized.expected)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff test_register_compatible_vectorized.expected test_register_compatible_vectorized.output)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test1.ml test1_classic.ml)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test1.mli test1_classic.mli)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (targets test1_classic_runner.exe test1_classic.cmx.dump)
 (deps test1_classic.mli test1_classic.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -S -O3 -g -dump-into-file -dcfg -dvectorize -dlinear -regalloc cfg -extension simd -vectorize-max-block-size 1000 -Oclassic -vectorize -o test1_classic_runner.exe)))

(rule
 (enabled_if (= %{context_name} "main"))
 (target test1_classic.cmx.dump.output)
 (deps ./filter.sh test1_classic.cmx.dump)
 (action
  (with-outputs-to
   %{target}
   (with-accepted-exit-codes 1
    (run %{deps})))))
