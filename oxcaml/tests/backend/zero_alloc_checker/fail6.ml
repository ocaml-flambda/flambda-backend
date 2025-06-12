exception Exn of int

external opaque_middle_end_only : 'a -> 'a = "%obj_magic"

let[@zero_alloc] test3 n =
  try raise (opaque_middle_end_only (Exn n)) with Exn n -> n
