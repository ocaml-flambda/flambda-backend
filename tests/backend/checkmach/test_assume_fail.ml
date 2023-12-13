let[@zero_alloc assume][@inline never][@specialise never][@local never] bar x =
  if x > 0 then failwith (Printf.sprintf "BOO %d!" x);
  (x+1,x)

let[@zero_alloc strict] foo x = bar x


let[@zero_alloc assume][@inline always] bar' x =
  if x > 0 then failwith (Printf.sprintf "BOO %d!" x);
  (x+1,x)

let[@zero_alloc strict] foo' x = bar' x
