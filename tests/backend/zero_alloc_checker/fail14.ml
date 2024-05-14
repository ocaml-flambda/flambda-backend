exception Exn of (int * int)

let pass x = raise (Exn (x,x))

(* function attributes still work *)
let[@zero_alloc strict] foo x = if x>0 then pass (x+1) else x+2

(* doesn't matter where in the file it appears *)
[@@@zero_alloc all]
