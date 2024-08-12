(* TEST
 readonly_files = "called_cu.ml";
 setup-ocamlopt.opt-build-env;
 flags = "-no-always-tco -regalloc cfg -regalloc-param IRC_SPILLING_HEURISTICS:flat-uses -cfg-analyze-tailcalls -Oclassic";
 all_modules = "${readonly_files} caller_cu.ml";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

let calls_foo n k = Called_cu.foo n k
