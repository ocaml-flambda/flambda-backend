let[@zero_alloc assume][@inline always] test1 x = (x,x)

let[@inline always] test2 x = (x,x)

(* passes *)
let[@zero_alloc] test3 b x =
  if b then test1 (x+1) else test1 (x*2)

(* fails *)
let[@zero_alloc] test4 b x =
  let y = if b then test1 (x+1) else test1 (x*2) in
  test2 y
