type t =
  | Thirty_two
  | Sixty_four

let of_int_exn = function
  | 32 -> Thirty_two
  | 64 -> Sixty_four
  | bits -> Misc.fatal_errorf "Unknown machine width: %d" bits
