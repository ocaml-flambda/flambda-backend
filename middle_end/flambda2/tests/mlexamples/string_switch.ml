type t =
  | Sheep
  | Cow
  | Goat
  | Horse

let of_string str =
  match str with
  | "Sheep" -> Sheep
  | "Cow" -> Cow
  | "Goat" -> Goat
  | "Horse" -> Horse
  | _ -> assert false
