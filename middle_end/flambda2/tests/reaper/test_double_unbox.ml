let glop () =
  let[@inline never] [@local never] f x y =
    let pair = x, y in
    let[@inline never] [@local never] closure_prem () =
      let a, b = pair in
      a + b
    in
    let[@inline never] [@local never] closure_second () =
      let a, b = pair in
      a * b
    in
    closure_prem () + closure_second ()
  in
  f 1 2
