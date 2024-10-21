(* T.test1 is never inlined and allocates. *)
let[@zero_alloc strict] test n = T4.test1 n
