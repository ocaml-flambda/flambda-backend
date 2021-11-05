(* Comes with closure_var_a.ml and closure_var_b.ml.

   In this example, closures created in Closure_var_b need to retain their
   closure variable [x] even though their bodies do not use it, because they
   might be passed to the parent code in Closure_var_a which does need the
   variable.

   By adding a third file that inlines the parent code ID, we check that the
   types for the closures also correctly include the variable (if they don't,
   the simplification of the Project_var primitive will return invalid and cause
   segfaults at runtime). *)

let f b =
  let g = if b then Closure_var_b.g0 else Closure_var_b.g1 in
  let x, y = (g [@inlined]) 3 in
  assert (x = 0 || x = 1);
  assert (y = 3);
  ()
