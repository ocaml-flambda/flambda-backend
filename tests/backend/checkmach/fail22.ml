(* expect to fail with -zero-alloc-check-opt *)
let[@zero_alloc opt] foo x = (x,x)

