let[@inline] f x = x + 1

let[@inline never] foo a =
  let x1 = a * 42 in
  let x2 = f a in
  let x3 = a * 100 in
  x1, x2 + x3

let () =
  print_endline (string_of_int (snd (foo 1)))
