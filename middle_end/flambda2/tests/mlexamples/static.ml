
let f x l =
  let g b = if b then x else 42 in
  let h acc x = acc + x + g false in
  List.fold_left h 0 l


