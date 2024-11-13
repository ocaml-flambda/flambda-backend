(* TEST
 flambda2;
 native;
*)

[@@@ocaml.flambda_o3]

external magic : 'a -> 'b = "%identity"

type _ tag =
  | A : int list tag
  | B : (int * int list option) tag

type mut = E : { mutable tag : 'a tag; v : 'a } -> mut

let go input =
  let (x, y) : int * int list option =
    match input with
    | E { tag = A; v = a } -> 0, Some a
    | E { tag = B; v = b } -> let (b0, b1) = b in (b0, b1)
  in
  x, y
;;

let make i =
  let list = [ i; 123 ] in
  if true then
    go (magic (A, list))
  else
    go (E { tag = A; v = list})
;;