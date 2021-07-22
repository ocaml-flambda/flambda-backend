type t =
  | A of int
  | B of int
  | C of int

let[@inline always] g t =
  match t with
  | A x -> x + 1
  | B x -> x * 2
  | C x -> x * 42

let bar x =
  let t =
    if x < 0 then A x
    else B x
  in
  g t
