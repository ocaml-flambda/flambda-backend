let[@inline always] bar x = (x,x)

let[@zero_alloc] test1 x =
  (bar[@zero_alloc]) x

let[@zero_alloc] test2 x =
  (bar[@zero_alloc assume strict]) x

let[@zero_alloc] test3 x =
  (bar[@zero_alloc assume never_returns_normally]) x
