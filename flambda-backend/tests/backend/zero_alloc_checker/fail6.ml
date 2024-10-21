exception Exn of int
let[@zero_alloc] test3 n =
  try raise (Exn n) with Exn n -> n
