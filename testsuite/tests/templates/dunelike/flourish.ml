type t = P.t * P.t

let create p = p, P.frob (P.create ())

let to_string (p1, p2) =
  "Flourish(" ^ P.to_string p1 ^ ", " ^ P.to_string p2 ^ ")"
