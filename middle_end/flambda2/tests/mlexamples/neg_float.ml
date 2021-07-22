let neg_float = (-42.0)

external (-.) : float -> float -> float = "%subfloat"

let make_pos_float () =
  42.0 -. 40.0

let make_neg_float () =
  42.0 -. 44.0
