[@@@zero_alloc all]

(* For duplicate attributes we get a warning and the first one takes effect, so the
   following function, which passes the relaxed test but not the strict, triggers
   failure of the check of [call_loud1]. *)
let[@inline never][@zero_alloc ignore][@zero_alloc assume] fails x = (x,x)

let[@zero_alloc strict] call x = fails (x+1)
