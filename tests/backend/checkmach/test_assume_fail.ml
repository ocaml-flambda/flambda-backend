let[@zero_alloc assume][@inline never][@specialise never][@local never] bar x =
  if x > 0 then failwith (Printf.sprintf "BOO %d!" x);
  (x+1,x)

let[@zero_alloc strict] foo x = bar x


let[@zero_alloc assume][@inline always] bar' x =
  if x > 0 then failwith (Printf.sprintf "BOO %d!" x);
  (x+1,x)

let[@zero_alloc strict] foo' x = bar' x

let[@inline always] test46 x = if x > 0 then failwith (Printf.sprintf "%d" x) else (x,x)

let[@zero_alloc strict] test48 x =
  (test46[@zero_alloc assume never_returns_normally]) x

(* Perhaps confusingly, never_returns_normally works on allocations not only
   on calls. This is needed for analysis to give the same results regardess
   of inlining of expressions annotated with "assume". See test50-53 below.  *)
let[@zero_alloc] test49 x =
  try let y = (test46[@zero_alloc assume never_returns_normally]) x in [y;(x,x+1)]
  with _ -> failwith (Printf.sprintf "%d" x)

(* strict check should fail *)
let[@inline always][@zero_alloc assume never_returns_normally] test50 x = (x, x)

let[@zero_alloc strict] test51 x = test50 x

let[@inline never][@zero_alloc assume never_returns_normally] test52 x = (x, x)

let[@zero_alloc strict] test53 x = test52 x

(* relaxed check should pass *)
let[@inline always][@zero_alloc assume never_returns_normally] test54 x = (x, x)

let[@zero_alloc] test55 x = test54 x

let[@inline never][@zero_alloc assume never_returns_normally] test56 x = (x, x)

let[@zero_alloc] test57 x = test56 x

(* strict assume should pass *)
let[@inline always][@zero_alloc assume strict never_returns_normally] test58 x = (x, x)

let[@zero_alloc strict] test59 x = test58 x

let[@inline never][@zero_alloc assume never_returns_normally strict] test60 x = (x, x)

let[@zero_alloc strict] test61 x = test60 x

(* allocations on the path to a call labeled [@zero_alloc assume never_returns_normally]
   do not count for the normal check, but do count for the strict check *)

let[@inline never] test62 x = [x;x]

let[@zero_alloc] test63 x =
  let p = [x;x+1] in
  (test62[@zero_alloc assume never_returns_normally]) p

let[@zero_alloc strict] test64 x =
  let p = [x;x+1] in
  (test62[@zero_alloc assume never_returns_normally]) p
