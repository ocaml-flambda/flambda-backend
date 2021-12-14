let f x y =
  let a = x + y in
  let p = x, y in
  let x' = fst p in
  let b = x' + y in
  a, b
