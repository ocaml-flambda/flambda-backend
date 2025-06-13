let glop () =
  let[@inline never] [@local never] f x y =
    let machin = 42, 42 in
    let pair = x, y, machin in
    let[@inline never] [@local never] closure () =
      let a, b, _ = pair in
      a + b
    in
    let () = () in
    closure
  in
  let cloclo = f 1 2 in
  let () = () in
  ignore (Sys.opaque_identity f);
  cloclo ()
