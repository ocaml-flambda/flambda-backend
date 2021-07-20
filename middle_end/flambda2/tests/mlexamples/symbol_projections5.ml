external getenv : string -> string = "caml_sys_getenv"
external (+) : int -> int -> int = "%addint"
external (<) : int -> int -> bool = "%lessthan"
external (&&) : bool -> bool -> bool = "%logand"

let foo =
  match getenv "FOO" with
  | exception _ -> false
  | _ -> true

type t =
  | S of (int -> t)
  | T of (int -> t)

let f b =
  if b then
    let rec g y =
      if y < 0 then S g
      else T g
    in
    g
  else
    let rec h z =
      if z < 0 then S h
      else T h
    in
    h
