(* expect to succeed with -zero-alloc-check-opt without -zero-alloc-check *)
let[@zero_alloc] bar x = (x,x)
let[@zero_alloc strict] test1 x = if x > 0 then x+1 else failwith "x must be positive"
