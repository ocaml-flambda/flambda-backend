(* output of errors wwhen zero_alloc check fails *)
let[@inline never] bar x = Sys.opaque_identity [x+1;x+x]

let[@inline never] pred b =
  Sys.opaque_identity b

let[@zero_alloc] foo b x f =
  let b = fst (pred b) in
  let y =
  if b then (x,x)
  else
    match bar x with
    | [c] -> f c
    | [a;_] -> [%probe "test" (print_int x)]; (a, a+x)
    | _ -> (x+x,x*x)
  in
  let t = (x,y) in
  (t,Dep19.inline_always x)
