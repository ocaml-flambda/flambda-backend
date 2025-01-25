(* TEST
 flags = "-g";
 include stdlib_upstream_compatible;
 bytecode;
*)

let f1 f i = Stdlib_upstream_compatible.Float_u.to_float (f i)
let f2 f i = Stdlib_upstream_compatible.Float_u.to_float (f i) +. 0.
