(* Checks that various operations preserve the order of declarations to the
   extent possible. Ideally the code declarations will appear in numerical order
   (which is slightly different from the order here since they will be sorted by
   SCC analysis). The closure bindings may be in the order here.

   NOTE: Currently the conversion into flambda2 does a good job preserving order
   but the simplifier does not. *)

let rec g0 () = ()

and g1 () = g2 ()

and g2 () = g1 ()

and g3 () = g1 ()

and g4 () = ()

and g6 () = g5 ()

and g7 () = g8 ()

and g5 () = g1 ()

and g8 () = g7 ()

and g9 () = ()
