let foo x = (x,x)

let[@zero_alloc] bar x = (foo[@zero_alloc assume_unless_opt]) x

let[@zero_alloc] test_warning x =
  (foo[@zero_alloc assume_unless_opt never_returns_normally]) x
