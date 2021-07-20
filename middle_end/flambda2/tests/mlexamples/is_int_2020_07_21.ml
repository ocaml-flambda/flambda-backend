type src = C1 of int | C2

type dst = A | B | C

let [@inline always] of_src = function
  | C1 _ -> A
  | C2 -> B

let test src f g =
  let dst =
    if f () then of_src src
    else C
  in
  g dst
