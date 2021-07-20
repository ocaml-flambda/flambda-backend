let[@inline] maybe b x = if b then Some x else None

let shouldn't_alloc f g b x =
  match maybe b x with
  | None -> g ()
  | Some x -> f x
