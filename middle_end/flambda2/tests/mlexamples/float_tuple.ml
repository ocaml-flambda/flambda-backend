
external (+.) : float -> float -> float = "%addfloat"

let f b x =
  let y =
    if b then
      x +. 1., 0.
    else
      x +. 42., x +. 1.
  in
  match y with
  | a, b -> a +. b


