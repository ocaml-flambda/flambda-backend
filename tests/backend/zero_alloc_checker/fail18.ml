let[@zero_alloc] f x ?(y = (x, x)) () = fst y + snd y
