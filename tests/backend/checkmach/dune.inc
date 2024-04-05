
(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps s.ml t.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc-checker -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps t5.ml test_assume.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc-checker -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_match_on_mutable_state.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc-checker -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_flambda.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc-checker -dump-into-file -O3 -warn-error +a)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail1.output.corrected)
 (deps (:ml fail1.ml) filter.sh)
 (action
   (with-outputs-to fail1.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail1.output fail1.output.corrected)
 (action (diff fail1.output fail1.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail2.output.corrected)
 (deps (:ml fail2.ml) filter.sh)
 (action
   (with-outputs-to fail2.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail2.output fail2.output.corrected)
 (action (diff fail2.output fail2.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail3.output.corrected)
 (deps (:ml t3.ml fail3.ml) filter.sh)
 (action
   (with-outputs-to fail3.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail3.output fail3.output.corrected)
 (action (diff fail3.output fail3.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail4.output.corrected)
 (deps (:ml t4.ml fail4.ml) filter.sh)
 (action
   (with-outputs-to fail4.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail4.output fail4.output.corrected)
 (action (diff fail4.output fail4.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail5.output.corrected)
 (deps (:ml fail5.ml) filter.sh)
 (action
   (with-outputs-to fail5.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail5.output fail5.output.corrected)
 (action (diff fail5.output fail5.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail6.output.corrected)
 (deps (:ml fail6.ml) filter.sh)
 (action
   (with-outputs-to fail6.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail6.output fail6.output.corrected)
 (action (diff fail6.output fail6.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail7.output.corrected)
 (deps (:ml fail7.ml) filter.sh)
 (action
   (with-outputs-to fail7.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail7.output fail7.output.corrected)
 (action (diff fail7.output fail7.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail8.output.corrected)
 (deps (:ml fail8.ml) filter.sh)
 (action
   (with-outputs-to fail8.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail8.output fail8.output.corrected)
 (action (diff fail8.output fail8.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail9.output.corrected)
 (deps (:ml fail9.ml) filter.sh)
 (action
   (with-outputs-to fail9.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail9.output fail9.output.corrected)
 (action (diff fail9.output fail9.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail10.output.corrected)
 (deps (:ml fail10.ml) filter.sh)
 (action
   (with-outputs-to fail10.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail10.output fail10.output.corrected)
 (action (diff fail10.output fail10.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail12.output.corrected)
 (deps (:ml fail12.ml) filter.sh)
 (action
   (with-outputs-to fail12.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail12.output fail12.output.corrected)
 (action (diff fail12.output fail12.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail13.output.corrected)
 (deps (:ml fail13.ml) filter.sh)
 (action
   (with-outputs-to fail13.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail13.output fail13.output.corrected)
 (action (diff fail13.output fail13.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail14.output.corrected)
 (deps (:ml fail14.ml) filter.sh)
 (action
   (with-outputs-to fail14.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail14.output fail14.output.corrected)
 (action (diff fail14.output fail14.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail15.output.corrected)
 (deps (:ml fail15.ml) filter.sh)
 (action
   (with-outputs-to fail15.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail15.output fail15.output.corrected)
 (action (diff fail15.output fail15.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail16.output.corrected)
 (deps (:ml fail16.ml) filter.sh)
 (action
   (with-outputs-to fail16.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail16.output fail16.output.corrected)
 (action (diff fail16.output fail16.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail17.output.corrected)
 (deps (:ml fail17.ml) filter.sh)
 (action
   (with-outputs-to fail17.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail17.output fail17.output.corrected)
 (action (diff fail17.output fail17.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail18.output.corrected)
 (deps (:ml fail18.ml) filter.sh)
 (action
   (with-outputs-to fail18.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail18.output fail18.output.corrected)
 (action (diff fail18.output fail18.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail19.output.corrected)
 (deps (:ml dep19.ml fail19.ml) filter.sh)
 (action
   (with-outputs-to fail19.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail19.output fail19.output.corrected)
 (action (diff fail19.output fail19.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail20.output.corrected)
 (deps (:ml fail20.ml) filter.sh)
 (action
   (with-outputs-to fail20.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail20.output fail20.output.corrected)
 (action (diff fail20.output fail20.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail21.output.corrected)
 (deps (:ml fail21.ml) filter.sh)
 (action
   (with-outputs-to fail21.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail21.output fail21.output.corrected)
 (action (diff fail21.output fail21.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_attribute_error_duplicate.output.corrected)
 (deps (:ml test_attribute_error_duplicate.ml) filter.sh)
 (action
   (with-outputs-to test_attribute_error_duplicate.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_attribute_error_duplicate.output test_attribute_error_duplicate.output.corrected)
 (action (diff test_attribute_error_duplicate.output test_attribute_error_duplicate.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_attr_unused.output.corrected)
 (deps (:ml test_attr_unused.ml) filter.sh)
 (action
   (with-outputs-to test_attr_unused.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_attr_unused.output test_attr_unused.output.corrected)
 (action (diff test_attr_unused.output test_attr_unused.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets t6.output.corrected)
 (deps (:ml t6.ml) filter.sh)
 (action
   (with-outputs-to t6.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps t6.output t6.output.corrected)
 (action (diff t6.output t6.output.corrected)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps t7.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc-checker -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_stub_dep.ml test_stub.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc-checker -dump-into-file -O3 -warn-error +a)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets t1.output.corrected)
 (deps (:ml t1.ml) filter.sh)
 (action
   (with-outputs-to t1.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps t1.output t1.output.corrected)
 (action (diff t1.output t1.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_warning199.output.corrected)
 (deps (:ml test_warning199.mli test_warning199.ml) filter.sh)
 (action
   (with-outputs-to test_warning199.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_warning199.output test_warning199.output.corrected)
 (action (diff test_warning199.output test_warning199.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_never_returns_normally.output.corrected)
 (deps (:ml test_never_returns_normally.ml) filter.sh)
 (action
   (with-outputs-to test_never_returns_normally.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_never_returns_normally.output test_never_returns_normally.output.corrected)
 (action (diff test_never_returns_normally.output test_never_returns_normally.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail22.output.corrected)
 (deps (:ml fail22.ml) filter.sh)
 (action
   (with-outputs-to fail22.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check opt -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail22.output fail22.output.corrected)
 (action (diff fail22.output fail22.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail23.output.corrected)
 (deps (:ml fail23.ml) filter.sh)
 (action
   (with-outputs-to fail23.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check opt -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail23.output fail23.output.corrected)
 (action (diff fail23.output fail23.output.corrected)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_zero_alloc_opt1.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check opt -dcse -dzero-alloc-checker -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_zero_alloc_opt2.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check opt -dcse -dzero-alloc-checker -dump-into-file -O3 -warn-error +a)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_assume_fail.output.corrected)
 (deps (:ml test_assume_fail.ml) filter.sh)
 (action
   (with-outputs-to test_assume_fail.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_assume_fail.output test_assume_fail.output.corrected)
 (action (diff test_assume_fail.output test_assume_fail.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_assume_on_call.output.corrected)
 (deps (:ml test_assume_on_call.ml) filter.sh)
 (action
   (with-outputs-to test_assume_on_call.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_assume_on_call.output test_assume_on_call.output.corrected)
 (action (diff test_assume_on_call.output test_assume_on_call.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_misplaced_assume.output.corrected)
 (deps (:ml test_misplaced_assume.ml) filter.sh)
 (action
   (with-outputs-to test_misplaced_assume.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_misplaced_assume.output test_misplaced_assume.output.corrected)
 (action (diff test_misplaced_assume.output test_misplaced_assume.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_misplaced_attr.output.corrected)
 (deps (:ml test_misplaced_attr.ml) filter.sh)
 (action
   (with-outputs-to test_misplaced_attr.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_misplaced_attr.output test_misplaced_attr.output.corrected)
 (action (diff test_misplaced_attr.output test_misplaced_attr.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_attr_check.output.corrected)
 (deps (:ml test_attr_check.ml) filter.sh)
 (action
   (with-outputs-to test_attr_check.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_attr_check.output test_attr_check.output.corrected)
 (action (diff test_attr_check.output test_attr_check.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_attr_check_all.output.corrected)
 (deps (:ml test_attr_check_all.ml) filter.sh)
 (action
   (with-outputs-to test_attr_check_all.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_attr_check_all.output test_attr_check_all.output.corrected)
 (action (diff test_attr_check_all.output test_attr_check_all.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_attr_check_opt.output.corrected)
 (deps (:ml test_attr_check_opt.ml) filter.sh)
 (action
   (with-outputs-to test_attr_check_opt.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_attr_check_opt.output test_attr_check_opt.output.corrected)
 (action (diff test_attr_check_opt.output test_attr_check_opt.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_attr_check_none.output.corrected)
 (deps (:ml test_attr_check_none.ml) filter.sh)
 (action
   (with-outputs-to test_attr_check_none.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_attr_check_none.output test_attr_check_none.output.corrected)
 (action (diff test_attr_check_none.output test_attr_check_none.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail24.output.corrected)
 (deps (:ml fail24.ml) filter.sh)
 (action
   (with-outputs-to fail24.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail24.output fail24.output.corrected)
 (action (diff fail24.output fail24.output.corrected)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_raise_message.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -function-layout topological -dcse -dzero-alloc-checker -dump-into-file -O3 -warn-error +a)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail25.output.corrected)
 (deps (:ml fail25.ml) filter.sh)
 (action
   (with-outputs-to fail25.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -disable-precise-zero-alloc-checker -function-layout source -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail25.output fail25.output.corrected)
 (action (diff fail25.output fail25.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail26.output.corrected)
 (deps (:ml fail26.ml) filter.sh)
 (action
   (with-outputs-to fail26.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -disable-zero-alloc-checker -function-layout source -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail26.output fail26.output.corrected)
 (action (diff fail26.output fail26.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_all_opt.output.corrected)
 (deps (:ml test_all_opt.ml) filter.sh)
 (action
   (with-outputs-to test_all_opt.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_all_opt.output test_all_opt.output.corrected)
 (action (diff test_all_opt.output test_all_opt.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_all_opt2.output.corrected)
 (deps (:ml test_all_opt2.ml) filter.sh)
 (action
   (with-outputs-to test_all_opt2.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check all -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_all_opt2.output test_all_opt2.output.corrected)
 (action (diff test_all_opt2.output test_all_opt2.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_all_opt3.output.corrected)
 (deps (:ml test_all_opt3.ml) filter.sh)
 (action
   (with-outputs-to test_all_opt3.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check opt -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_all_opt3.output test_all_opt3.output.corrected)
 (action (diff test_all_opt3.output test_all_opt3.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_arity.output.corrected)
 (deps (:ml test_arity.ml) filter.sh)
 (action
   (with-outputs-to test_arity.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_arity.output test_arity.output.corrected)
 (action (diff test_arity.output test_arity.output.corrected)))

(rule
 (alias runtest)
 (deps stop_after_typing.ml)
 (target stop_after_typing.cmi)
 (enabled_if (= %{context_name} "main"))
 (action (run %{bin:ocamlopt.opt} stop_after_typing.ml -g -c -opaque -stop-after typing -O3 -warn-error +a)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_signatures_functors.output.corrected)
 (deps (:ml test_signatures_functors.ml) filter.sh)
 (action
   (with-outputs-to test_signatures_functors.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_signatures_functors.output test_signatures_functors.output.corrected)
 (action (diff test_signatures_functors.output test_signatures_functors.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_signatures_first_class_modules.output.corrected)
 (deps (:ml test_signatures_first_class_modules.ml) filter.sh)
 (action
   (with-outputs-to test_signatures_first_class_modules.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_signatures_first_class_modules.output test_signatures_first_class_modules.output.corrected)
 (action (diff test_signatures_first_class_modules.output test_signatures_first_class_modules.output.corrected)))

(rule
 (alias runtest)
 (deps test_signatures_separate_a.ml)
 (target test_signatures_separate_a.cmi)
 (enabled_if (= %{context_name} "main"))
 (action (run %{bin:ocamlopt.opt} test_signatures_separate_a.ml -g -c -opaque -stop-after typing -O3 -warn-error +a)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_signatures_separate.output.corrected)
 (deps test_signatures_separate_a.cmi (:ml test_signatures_separate_b.ml) filter.sh)
 (action
   (with-outputs-to test_signatures_separate.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_signatures_separate.output test_signatures_separate.output.corrected)
 (action (diff test_signatures_separate.output test_signatures_separate.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_signatures_separate.opt.output.corrected)
 (deps test_signatures_separate_a.cmi (:ml test_signatures_separate_b.ml) filter.sh)
 (action
   (with-outputs-to test_signatures_separate.opt.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check all -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_signatures_separate.opt.output test_signatures_separate.opt.output.corrected)
 (action (diff test_signatures_separate.opt.output test_signatures_separate.opt.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_assume_inlining.output.corrected)
 (deps (:ml test_assume_inlining.ml) filter.sh)
 (action
   (with-outputs-to test_assume_inlining.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_assume_inlining.output test_assume_inlining.output.corrected)
 (action (diff test_assume_inlining.output test_assume_inlining.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_assume_error.output.corrected)
 (deps (:ml test_assume_error.ml) filter.sh)
 (action
   (with-outputs-to test_assume_error.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_assume_error.output test_assume_error.output.corrected)
 (action (diff test_assume_error.output test_assume_error.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_assume_stub.output.corrected)
 (deps (:ml test_assume_stub.ml) filter.sh)
 (action
   (with-outputs-to test_assume_stub.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_assume_stub.output test_assume_stub.output.corrected)
 (action (diff test_assume_stub.output test_assume_stub.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_bounded_join.output.corrected)
 (deps (:ml test_bounded_join.ml) filter_fatal_error.sh)
 (action
   (with-outputs-to test_bounded_join.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-join -2 -checkmach-details-cutoff 20 -O3))
    (run "./filter_fatal_error.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_bounded_join.output test_bounded_join.output.corrected)
 (action (diff test_bounded_join.output test_bounded_join.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_bounded_join2.output.corrected)
 (deps (:ml test_bounded_join2.ml) filter.sh)
 (action
   (with-outputs-to test_bounded_join2.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-join 2 -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_bounded_join2.output test_bounded_join2.output.corrected)
 (action (diff test_bounded_join2.output test_bounded_join2.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_bounded_join3.output.corrected)
 (deps (:ml test_bounded_join3.ml) filter.sh)
 (action
   (with-outputs-to test_bounded_join3.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-join 0 -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_bounded_join3.output test_bounded_join3.output.corrected)
 (action (diff test_bounded_join3.output test_bounded_join3.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_bounded_join4.output.corrected)
 (deps (:ml test_bounded_join4.ml) filter.sh)
 (action
   (with-outputs-to test_bounded_join4.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-join 0 -checkmach-details-cutoff 3 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_bounded_join4.output test_bounded_join4.output.corrected)
 (action (diff test_bounded_join4.output test_bounded_join4.output.corrected)))
