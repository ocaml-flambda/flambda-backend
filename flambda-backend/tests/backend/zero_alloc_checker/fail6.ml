exception Exn of int
let[@zero_alloc] test3 n =
  try raise (Obj.magic (Exn n)) with Exn n -> n
