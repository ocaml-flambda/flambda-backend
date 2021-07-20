(*
type t =
  | A
  | B of int
  | C of int

let f () =
  let x = A in
  match x with
  | A -> 0
  | B _ -> 1
  | C _ -> 2

let g () =
  let x = B 42 in
  match x with
  | A -> 0
  | B _ -> 1
  | C _ -> 2
*)

external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"

let rec f x =
  if x > 0 then 1 + f (x - 1)
  else 42

let n = (f [@unrolled 10]) 5
