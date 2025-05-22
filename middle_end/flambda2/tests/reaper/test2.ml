let glop () =
  let[@inline never] [@local never] f x y =
    let machin = 42, 42 in
    let pair = x, y, machin in
    let pepere = pair, machin in
    let[@inline never] [@local never] closure () =
      let repere, _ = pepere in
      let a, b, _ = repere in
      a + b
    in
    let () = () in
    closure
  in
  let cloclo = f 1 2 in
  let () = () in
  cloclo ()
