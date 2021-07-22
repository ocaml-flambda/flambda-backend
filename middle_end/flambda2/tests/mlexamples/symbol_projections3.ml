external getenv : string -> string = "caml_sys_getenv"
external (+) : int -> int -> int = "%addint"

let foo =
  match getenv "FOO" with
  | exception _ -> false
  | _ -> true

let f x =
  let g y =
    if foo then y + y
    else y
  in
  let block_to_lift = foo, foo in
  x, g, block_to_lift
