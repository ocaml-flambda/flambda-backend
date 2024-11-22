
type t = A | B | C | D
type view = Foo of int | Bar of int

let[@inline never] int () = 0

let view i =
  match i with
  | A -> Foo 0
  | B -> Foo (int ())
  | C -> Bar 13
  | D -> Bar 42

let foo i =
  match view i with
  | Foo i -> Format.printf "foo: %d" i
  | Bar 13 -> Format.printf "bar/13"
  | Bar 42 -> Format.printf "bar/42"
  | Bar _ -> assert false (* can this branch be deleted ? *)

(*

type t = int

type view =
  | Negative
  | Zero
  | Odd of int
  | Even of int

let view i =
  if i < 0 then Negative
  else if i = 0 then Zero
  else if i mod 2 = 0 then Even (i / 2)
  else Odd ((i - 1) / 2)

(*  a convoluted way to write the identity *)
let foo i =
  match view i with
  | Negative -> i
  | Zero -> 0
  | Odd n -> 2 * n + 1
  | Even n -> 2 * n

*)


