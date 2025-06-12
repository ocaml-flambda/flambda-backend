(* Parameters: P, Q *)

(* This is in the [fancy] library, so it's compiled with
[-open Fancy__ -open No_direct_access_to_fancy] *)

type t = string

let create t = t
let to_string t = "Ornament(" ^ t ^ ")"
