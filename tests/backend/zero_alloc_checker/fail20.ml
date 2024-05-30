exception Exn of int * int

let exn x = raise (Exn (x,x))
let rec div x : int list = x::(div (x+1))
let[@inline never] nor x = x+1
(* test detailed output for exceptions and diverge *)
let[@zero_alloc strict] foo1 x =
  if x > 0 then exn (x+1)
  else if x < 0 then
    List.nth (div (x+2)) x
  else nor x

let[@zero_alloc][@inline never][@local never]  foo x =
  let[@inline never][@local never] bar x y =
    (Sys.opaque_identity (x + y), x)
  in
  fst (bar x (x*x))

let[@zero_alloc] zee x f = foo (Sys.opaque_identity x+1)
