let[@zero_alloc][@inline always] baz y = snd y

let[@zero_alloc] foo x = baz (x-1,x)
