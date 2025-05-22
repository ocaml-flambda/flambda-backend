let chocho momo doudou =
  let[@inline never] [@local never] glouglou x y =
    let[@inline never] [@local never] plop a b = fst a + fst b in
    if Sys.opaque_identity true then plop x (12, 12) else plop y y
  in
  let () = () in
  glouglou momo doudou
