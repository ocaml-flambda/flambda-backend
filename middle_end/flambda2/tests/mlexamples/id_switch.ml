type t0 =
  | A
  | B
  | C

type t1 =
  | X
  | Y
  | Z

let [@inline always] x xt0 =
  match xt0 with
  | A -> X
  | B | C -> Y

let id_switch t0 =
  match t0 with
  | A -> x t0
  | B -> Y
  | C -> Z

type t2 =
  | M
  | N

let not_switch t2 =
  match t2 with
  | M -> B
  | N -> A
