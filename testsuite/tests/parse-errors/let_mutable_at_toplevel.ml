(* TEST
   flags = "-extension let_mutable";
   toplevel; *)

(* let mutable not allowed at structure level *)
let mutable x = 10
