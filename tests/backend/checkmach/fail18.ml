let escape (x : 'a) = Sys.opaque_identity (x : 'a); ()

let[@zero_alloc] f x ?(y = (x, x)) () = escape y; fst y + snd y
