external op : 'a -> 'a = "%opaque"

module A = Closure_var_use_a

let _ =
  let g =
    if op true then (A.f [@inlined always]) 0 else (A.f [@inlined always]) 1
  in
  (* Here the join should give us a type for [g] that is a known closure,
     with its code ID being the common ancestor between the two branches,
     i.e. the original code ID from [A] for the anonymous function.
     The new code IDs created in each branch do not need the [x] closure
     variable, as it is a known constant in both cases, but the ancestor
     still needs it.
     This test will fail if we remove the closure variable [x] from the
     closure allocations in the branches. (Note that inlining [g] would
     introduce a projection in this compilation unit, which would
     obviously ensure that we don't remove the closure variable).
  *)
  (g [@inlined never]) 2
