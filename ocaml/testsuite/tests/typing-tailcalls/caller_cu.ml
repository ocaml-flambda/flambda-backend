(* TEST
 readonly_files = "called_cu.ml";
 setup-ocamlopt.opt-build-env;
 flags = "-no-always-tco -regalloc cfg -regalloc-param IRC_SPILLING_HEURISTICS:flat-uses -cfg-analyze-tailcalls";
 all_modules = "${readonly_files} caller_cu.ml";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* Test cross CU warnings (we expose whether a function (transitively)
   tailcalls <unknown> in the CMX). *)

let rec calls_tailcalls_arg () =
  Called_cu.tailcalls_arg ~fn:(fun _ -> calls_tailcalls_arg ())

(* This stack overflows at runtime, but there should be no warning because
   doesn't_tailcall_arg does not tailcall its arg (i.e. before less-tco, it
   is already stack overflows, so we are not breaking anything). *)
let rec calls_doesn't_tailcall_arg () =
  Called_cu.doesn't_tailcall_arg ~fn:(fun _ -> calls_doesn't_tailcall_arg ())
