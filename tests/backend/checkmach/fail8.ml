(* forward dependency on g *)
exception E
let[@zero_alloc] rec f x b =
  if b then (g x; raise E)
  else ()
and g y = ignore (Sys.opaque_identity (y, y)) (* Allocating *)
