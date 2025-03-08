external (+.) : float -> float -> float = "%addfloat"
external opaque : 'a -> 'a = "%opaque"

let f x =
  let[@local] g a =
    if opaque false then a +. 1. else a +. 2.
  in
  if opaque false then g x else g (x +. 3.)