let[@inline always] bar x = (x,x)

let[@zero_alloc] test1 x =
  (bar[@zero_alloc]) x
