let[@inline] f x = x + 1

let[@inline never] foo a =
  let x1 = a + 42 in
  let x1 = Sys.opaque_identity x1 in
  let x2 = f a in
  let x2 = Sys.opaque_identity x2 in
  let x3 = a + 100 in
  x1, x2 + x3

let () =
  (print_endline [@inlined never])
    ((string_of_int [@inlined never]) (snd (foo 1)))
