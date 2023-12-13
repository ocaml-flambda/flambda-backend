(* TEST
   * bytecode
   flags = "-g -extension layouts"
 *)

let f1 f i = Stdlib__Float_u.to_float (f i)
let f2 f i = Stdlib__Float_u.to_float (f i) +. 0.
