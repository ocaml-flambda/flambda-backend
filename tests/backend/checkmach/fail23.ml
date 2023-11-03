exception Exn of string * int
(* expect to fail with -zero-alloc-check-opt *)
let[@zero_alloc opt strict] test1 x =
  if x > 0
  then x+1
  else
    raise (Exn ("expected positive", x))

