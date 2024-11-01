(* Warn about unused attributes and unchecked zero_alloc annotations. *)
let[@inline never] f x = if x > 0 then (Printf.eprintf "%d\n%!" x; raise Not_found)
let[@zero_alloc] g x = f x; (x, x)
let h x = g x
[@@@zero_alloc]
let[@zero_alloc] g x = (x[@zero_alloc])
