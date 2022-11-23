(* Comes with value_slot_a.ml and value_slot_b.ml.

   In this example, closures created in Value_slot_b need to retain their value
   slot [x] even though their bodies do not use it, because they might be passed
   to the parent code in Value_slot_a which does need the variable.

   By adding a third file that inlines the parent code ID, we check that the
   types for the closures also correctly include the variable (if they don't,
   the simplification of the Project_value_slot primitive will return invalid
   and cause segfaults at runtime). *)

let f b =
  let g = if b then Value_slot_b.g0 else Value_slot_b.g1 in
  let x, y = (g [@inlined]) 3 in
  assert (x = 0 || x = 1);
  assert (y = 3);
  ()
