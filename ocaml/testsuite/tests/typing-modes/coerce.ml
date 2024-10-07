(* TEST
   expect;
*)

(* We must put this in a module to stop the top-level from squeezing out
   unconstrained mode variables *)

module M = struct
  let f = fun _ -> ()

  let _ = (f :> 'a -> unit)

  let _ = f (local_ 42.0)
end

[%%expect{|
Line 6, characters 12-25:
6 |   let _ = f (local_ 42.0)
                ^^^^^^^^^^^^^
Error: This value escapes its region.
|}]
