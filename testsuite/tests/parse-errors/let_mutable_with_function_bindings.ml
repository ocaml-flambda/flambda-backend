(* TEST
   flags = "-extension let_mutable";
   toplevel; *)

(* let mutable not allowed with function bindings *)
let _ = let mutable f x = x in f;;
