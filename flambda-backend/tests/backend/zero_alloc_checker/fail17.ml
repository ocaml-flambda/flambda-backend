let escape (x : 'a) = Sys.opaque_identity (x : 'a); ()

[@@@zero_alloc all]
let f x ?(y = (x, x)) () = escape y; fst y + snd y
