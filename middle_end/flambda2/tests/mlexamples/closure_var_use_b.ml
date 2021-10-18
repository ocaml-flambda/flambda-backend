external op : 'a -> 'a = "%opaque"

module A = Closure_var_use_a

let _ =
  let g =
    if op true then (A.f [@inlined always]) 0 else (A.f [@inlined always]) 1
  in
  (g [@inlined never]) 2
