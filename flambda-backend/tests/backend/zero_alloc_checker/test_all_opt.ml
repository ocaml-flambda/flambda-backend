[@@@zero_alloc all]

(* check fails with -check-zero-alloc opt/all, passes with -check-zero-alloc default *)
let[@zero_alloc opt] foo x = (x,x)

(* check fails with -check-zero-alloc default/all, passes with "-check-zero-alloc opt" *)
let[@zero_alloc] bar x = [x;x]

let baz x = [|x;x+1|]
