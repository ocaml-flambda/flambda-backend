(* This just checks we don't get a warning 199 (unchecked zero alloc attribute)
   if we stop after typing. *)

let[@zero_alloc] f x = x
