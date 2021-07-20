let add_univars =
  List.fold_left (fun s x -> s + x)

let foo x y =
  add_univars x y
