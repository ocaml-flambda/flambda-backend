
type t =
  | Foo of int
  | Bar of string

let f r =
  let v =
    match r with
    | (Foo x') as res -> res
    | Bar _ -> raise Exit
  in
  let v' =
    if Sys.opaque_identity false then
      v
    else
      Foo 42
  in
  match v' with
  | Foo i -> i
  | Bar _ -> assert false


