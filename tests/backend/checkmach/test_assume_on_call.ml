let[@inline always] bar x = (x,x)

(* The following functions currently report misplaced attribute warnings.
   They are expected to compile successfully and pass the check
   when zero_alloc annotations on function calls are supported. *)

let[@zero_alloc] test1 x =
  (bar[@zero_alloc]) x

let[@zero_alloc] test2 x =
  (bar[@zero_alloc assume strict]) x

let[@zero_alloc] test3 x =
  (bar[@zero_alloc assume never_returns_normally]) x
