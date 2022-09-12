(* T.test1 is never inlined and allocates. *)
let[@assert noalloc] test n = T4.test1 n
