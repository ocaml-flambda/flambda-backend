external opaque : 'a -> 'a = "%opaque"
external (+) : int -> int -> int = "%addint"

type t =
| C0
| C1
| B0 of int * int
| B1 of int

type r = { f: t; }

let f x y =
  match (x.f, y.f) with
  | (_, C0) -> None
  | (B1 x, B1 y) ->
    Some (opaque x, opaque y)
  | (B0 (x1, x2), B0 (y1, y2)) ->
    Some (opaque (x1+x2), opaque (y1+y2))
  | (C1, C1) -> None
  | (_, _) -> Some (opaque (0, 0))
[@@inline never]

let _ = f {f = B0 (0, 0)} {f = B0 (0,0)}
