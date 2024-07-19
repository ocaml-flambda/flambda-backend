(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-no-always-tco -dlambda -dno-unique-ids";
 ocamlopt.opt;
 {
   stack-allocation;
   compiler_reference2 = "${test_source_directory}/tail.stack.reference";
   check-ocamlopt.opt-output;
 }{
   no-stack-allocation;
   compiler_reference2 = "${test_source_directory}/tail.heap.reference";
   check-ocamlopt.opt-output;
 }
*)

(* 
 flags = "-no-always-tco -dtypedtree -dlambda -dno-unique-ids -flambda2-inline-threshold 0 -inline 0"; *)
(* These calls should be inferred as tail-calls because they call, in tail 
   position, a function defined in some ancestor let rec. *)
let rec foo n =
  if n > 0 then foo (n - 2) else ()

let rec bar n =
  if n > 0 then baz (n - 2) else ()
and baz n = bar (n + 1)

(* (less-tco) The current heuristic used in -no-always-tco (incorrectly) does not
   infer tailcalls for the tail-position application of `next n k`.
   This is bad, since it blows the stack. *)
let rec collatz n k =
  let[@inline never] next n k =
    let n = n - 1
    and k = if k mod 2 == 0 then (k / 2) else ((3 * k) + 1) in
    collatz n k
  in
  (* Without this `ignore` call, the compiler inlines next (despite the
     [@inline never] annotation) which means the call does not show up (as applynontail)
     in the lambda output. `ignore` is a workaround. *)
  ignore next;
  if n > 0 then next n k else k
