(* Comes with closure_var_b.ml and closure_var_c.ml.

   In this example, closures created in Closure_var_b need to retain their
   closure variable [x] even though their bodies do not use it, because they
   might be passed to the parent code in Closure_var_a which does need the
   variable.

   By adding a third file that inlines the parent code ID, we check that the
   types for the closures also correctly include the variable (if they don't,
   the simplification of the Project_var primitive will return invalid and cause
   segfaults at runtime). *)

let f x =
  let () = () in
  let g y = x, y in
  g
