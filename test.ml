

let foo a b =
  let aux x =
    let y = Some min in
    if Sys.opaque_identity false then
      x, y
    else
      y, x
  in
  if Sys.opaque_identity false then
    aux a
  else
    aux b

let bar a b =
  (foo[@inlined]) a b
