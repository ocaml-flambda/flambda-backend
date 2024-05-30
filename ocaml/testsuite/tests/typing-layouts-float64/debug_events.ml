(* TEST
 flags = "-g";
 include stable;
 bytecode;
*)

let f1 f i = Stable.Float_u.to_float (f i)
let f2 f i = Stable.Float_u.to_float (f i) +. 0.
