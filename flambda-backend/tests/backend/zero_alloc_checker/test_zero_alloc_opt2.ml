(* expect to succeed with -zero-alloc-check-opt *)
let[@zero_alloc opt] test1 x = if x > 0 then x+1 else failwith "x must be positive"
let[@zero_alloc opt strict] test2 x = x + 1
let[@zero_alloc strict opt] test3 x = x - 1
