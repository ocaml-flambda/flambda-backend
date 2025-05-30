(* TEST
   flags = "-extension let_mutable";
   toplevel; *)

(* let mutable not allowed at structure level *)
module M = struct
  let mutable x = 20
end
