
external (+.) : float -> float -> float = "%addfloat"

let[@inline never] g x = x +. 42.

let f b x =
  let y = g x
(*
    if b then
      x +. 1.
    else
   g x
*)
  in
  y +. 0.


