
(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail1.output.corrected)
 (deps (:ml fail1.ml) filter.sh)
 (action
   (with-outputs-to fail1.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
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
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
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
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
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
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
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
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
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
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
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
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
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
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
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
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
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
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail10.output fail10.output.corrected)
 (action (diff fail10.output fail10.output.corrected)))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets fail11.output.corrected)
 (deps (:ml fail11.ml) filter.sh)
 (action
   (with-outputs-to fail11.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fail11.output fail11.output.corrected)
 (action (diff fail11.output fail11.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (targets fail12.output.corrected)
 (deps (:ml fail12.ml) filter.sh)
 (action
   (with-outputs-to fail12.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))
 (deps fail12.output fail12.output.corrected)
 (action (diff fail12.output fail12.output.corrected)))
