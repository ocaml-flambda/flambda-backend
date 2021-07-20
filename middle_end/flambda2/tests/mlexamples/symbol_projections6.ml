external getenv : string -> string = "caml_sys_getenv"
external (+) : int -> int -> int = "%addint"
external (<) : int -> int -> bool = "%lessthan"
external (&&) : bool -> bool -> bool = "%sequand"

let foo =
  match getenv "FOO" with
  | exception _ -> false
  | _ -> true

type t =
  | S of (int -> (t * bool))
  | T of (int -> (t * bool))

let f b =
  if b then
    let rec g y =
      if y < 0 && foo then S g, foo
      else T g, foo
    in
    g
  else
    let rec h z =
      if z < 0 && foo then S h, foo
      else T h, foo
    in
    h
