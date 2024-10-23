(* TEST *)

(* If stack allocation is enabled, the record will be allocated locally.
   This is allowed by the type-checker, but the dissect_letrec code
   choked on it. *)
type t = { a : int }
let rec x = let _ = { a = x } in 3
