external opaque : 'a -> 'a = "%opaque"
external raise : exn -> 'a = "%raise"
external (+) : int -> int -> int = "%addint"

exception E1
exception E2

let _ =
  let x, y =
    try
      try
        if opaque true then raise (opaque E1)
        else (0, 1)
      with E1 ->
        if opaque true then raise (opaque E2)
        else (2, 3)
    with E2 -> (4, 5)
  in
  opaque (x + y)
