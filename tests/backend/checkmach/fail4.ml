(* T.test1 is never inlined and allocates. *)
let[@noalloc] test n = T4.test1 n
