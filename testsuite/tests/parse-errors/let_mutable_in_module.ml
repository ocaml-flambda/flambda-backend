(* TEST_BELOW *)

(* let mutable not allowed at structure level *)
module M = struct
  let mutable x = 20
end

(* TEST
   flags = "-extension let_mutable";
   toplevel; *)
