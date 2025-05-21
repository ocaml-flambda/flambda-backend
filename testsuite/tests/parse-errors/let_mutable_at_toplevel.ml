(* TEST_BELOW *)

(* let mutable not allowed at structure level *)
let mutable x = 10

(* TEST
   flags = "-extension let_mutable";
   toplevel; *)
