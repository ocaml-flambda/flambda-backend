let glop c a b =
  let pair = a, b in
  let impair = b, a in
  let perdu = if c then pair else impair in
  let[@inline never] [@local never] fermeture () =
    let x, y = perdu in
    x + y
  in
  let _saucisse = Sys.opaque_identity impair in
  fermeture ()
