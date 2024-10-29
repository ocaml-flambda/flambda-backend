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
module M : sig val f : local_ 'a -> unit end
|}]
