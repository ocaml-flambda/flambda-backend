(* Parameters: P, Q *)

(* This is in the [fancy] library, so it's compiled with
[-open Fancy__ -open No_direct_access_to_fancy] *)

type t = P.t * P.t

let create p = p, P.frob (P.create ())

let to_string (p1, p2) =
  "Flourish(" ^ P.to_string p1 ^ ", " ^ P.to_string p2 ^ ")"
