external ( + ) : int -> int -> int = "%addint"

external id : 'a -> 'a = "%opaque"

let f1 b x =
  let opt = if b then None else Some x in
  match opt with None -> 1 | Some x -> x

type t =
  | A
  | B
  | C of int
  | D of int * int

let f2 x y a b =
  let t =
    match x, y with
    | true, true -> A
    | true, false -> B
    | false, true -> C a
    | false, false ->
      let toto = if id false then C b else D (a, b) in
      (* let _ = id () in *)
      toto
  in
  match t with C a -> a | D (a, b) -> a + b | A -> 40 | B -> 50

(* Should work with or without unboxing *)
let f3_returning_a x y a b =
  let t =
    match x, y with
    | true, true -> A
    | true, false -> B
    | false, true -> C a
    | false, false -> D (b, a)
  in
  match t with C a -> a | D (_, a) -> a | A | B -> a
