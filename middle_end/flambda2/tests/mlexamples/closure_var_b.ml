(* Comes with value_slot_a.ml and value_slot_c.ml.

   In this example, closures created in Closure_var_b need to retain their value
   slot [x] even though their bodies do not use it, because they might be passed
   to the parent code in Closure_var_a which does need the variable.

   By adding a third file that inlines the parent code ID, we check that the
   types for the closures also correctly include the variable (if they don't,
   the simplification of the Project_value_slot primitive will return invalid
   and cause segfaults at runtime). *)

let g0 = (Closure_var_a.f [@inlined]) 0

let g1 = (Closure_var_a.f [@inlined]) 1
