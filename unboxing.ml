let[@inline] some_if_pos x = if x > 0 then Some x else None
let[@cold] match_in_match x =
  match some_if_pos x with
  | Some x -> x * 2
  | None -> 17
;;

let[@inline] some_if_pos' x = if x > 0 then Some (x + 1) else None
let[@cold] match_in_match' x =
  match some_if_pos' x with
  | Some x -> x * 2
  | None -> 17
;;


type t =
  | C
  | D
  | E

type s =
  | A of int
  | B of int

let foo c a b =
  let m =
    match c with
    | C -> A a
    | D -> B b
    | E -> B (b + 1)
  in
  match m with
  | A x -> x
  | B y -> y

let bar m =
  match m with
  | A x -> x
  | B y -> y

let baz c a b =
  let m =
    match c with
    | C -> A a
    | D -> B b
    | E -> B (b + 1)
  in
  match Sys.opaque_identity m with
  | A x -> x
  | B y -> y
