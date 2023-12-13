let[@zero_alloc assume] foo =
  let x = 42 in
  fun z -> z + x

let[@zero_alloc] bar =
  let x = 42 in
  fun z -> z + x
