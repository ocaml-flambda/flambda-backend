let f x =
  let () = () in
  fun y -> match x with | 0 | 1 -> (x, y) | _ -> assert false
