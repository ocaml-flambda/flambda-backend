[@@@zero_alloc all]
let f x ?(y = (x, x)) () = fst y + snd y
