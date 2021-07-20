external ( + ) : int -> int -> int = "%addint"

type t =
  | A0
  | A1
  | B of int
  | C of int * int

let f t =
  match t with
  | A0 ->
    begin match t with
    | A0 -> 42
    | A1 -> 43
    | B _ -> 44
    | C _ -> 45
    end
  | A1 -> 0
  | B x -> x
  | C (a, b) -> a + b
