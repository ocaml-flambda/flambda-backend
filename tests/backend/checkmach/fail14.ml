exception Exn of (int * int)

[@@@zero_alloc all on]
let pass x = raise (Exn (x,x))

[@@@zero_alloc all off]
let boom x y z = [x;y;z]

[@@@zero_alloc all on]
(* function attributes still work *)
let[@zero_alloc strict] foo x = if x>0 then pass (x+1) else x+2
