[@@@warnings "+53"]

let[@inline never] bar x = (x,x)

let[@zero_alloc] foo x =
  ((bar x)[@zero_alloc assume])
