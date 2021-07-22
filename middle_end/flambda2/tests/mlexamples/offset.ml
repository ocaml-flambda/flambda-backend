
let[@inline always] inline a b =
  let r = Random.int 10 in
  let s = Random.int 10 in

  let
    rec f x = if a then r + x else 0
    and g x y = if b then s + x * y else 0
    and h x y =
      (if a then f (x + y) else 0)
      + (if b then g x y else 0)
  in
  h


(* let foo = inline true true *)
let bar = inline true false
let foobar = inline false false

