
(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps s.ml t.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dcheckmach -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps t5.ml test_assume.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dcheckmach -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_match_on_mutable_state.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dcheckmach -dump-into-file -O3)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (deps test_flambda.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dcheckmach -dump-into-file -O3 -warn-error +a)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail1.output.corrected)
 (deps (:ml fail1.ml) filter.sh)
 (action
   (with-outputs-to fail1.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
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
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail10.output fail10.output.corrected)
 (action (diff fail10.output fail10.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (targets fail12.output.corrected)
 (deps (:ml fail12.ml) filter.sh)
 (action
   (with-outputs-to fail12.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail18.output fail18.output.corrected)
 (action (diff fail18.output fail18.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (targets fail19.output.corrected)
 (deps (:ml dep19.ml fail19.ml) filter.sh)
 (action
   (with-outputs-to fail19.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (deps fail19.output fail19.output.corrected)
 (action (diff fail19.output fail19.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (targets fail20.output.corrected)
 (deps (:ml fail20.ml) filter.sh)
 (action
   (with-outputs-to fail20.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (deps fail20.output fail20.output.corrected)
 (action (diff fail20.output fail20.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (targets fail21.output.corrected)
 (deps (:ml fail21.ml) filter.sh)
 (action
   (with-outputs-to fail21.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
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
          -zero-alloc-check default -checkmach-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_attribute_error_duplicate.output test_attribute_error_duplicate.output.corrected)
 (action (diff test_attribute_error_duplicate.output test_attribute_error_duplicate.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (targets test_attr_unused.output.corrected)
 (deps (:ml test_attr_unused.ml) filter.sh)
 (action
   (with-outputs-to test_attr_unused.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
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
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
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
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dcheckmach -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_stub_dep.ml test_stub.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dcheckmach -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (deps t1.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dcheckmach -dump-into-file -O3 -warn-error +a)))

(rule
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (targets test_warning199.output.corrected)
 (deps (:ml test_warning199.mli test_warning199.ml) filter.sh)
 (action
   (with-outputs-to test_warning199.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (deps test_warning199.output test_warning199.output.corrected)
 (action (diff test_warning199.output test_warning199.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (targets test_never_returns_normally.output.corrected)
 (deps (:ml test_never_returns_normally.ml) filter.sh)
 (action
   (with-outputs-to test_never_returns_normally.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
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
          -zero-alloc-check opt -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail22.output fail22.output.corrected)
 (action (diff fail22.output fail22.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (targets fail23.output.corrected)
 (deps (:ml fail23.ml) filter.sh)
 (action
   (with-outputs-to fail23.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check opt -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (deps fail23.output fail23.output.corrected)
 (action (diff fail23.output fail23.output.corrected)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_zero_alloc_opt1.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check opt -dcse -dcheckmach -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_zero_alloc_opt2.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check opt -dcse -dcheckmach -dump-into-file -O3 -warn-error +a)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_assume_fail.output.corrected)
 (deps (:ml test_assume_fail.ml) filter.sh)
 (action
   (with-outputs-to test_assume_fail.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
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
          -zero-alloc-check all -checkmach-details-cutoff 20 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
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
          -zero-alloc-check default -checkmach-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps test_attr_check_none.output test_attr_check_none.output.corrected)
 (action (diff test_attr_check_none.output test_attr_check_none.output.corrected)))
