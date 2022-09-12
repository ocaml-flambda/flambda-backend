(* These checks fail with closure but pass with flambda and flambda2, so we have a
   separate file for them. *)
let[@inline always] test4 n = (n+1,n)

let[@assert noalloc][@inline never] test5 n =
  let first,_ = (test4 (n + 1)) in
  first
