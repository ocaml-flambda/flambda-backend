external nosimp : 'a -> 'a = "%obj_magic"

external ( + ) : int -> int -> int = "%addint"

external opaque : 'a -> 'a = "%opaque"

let test x =
  let y = x, x in
  let[@inline never] [@local never] clo () = y in
  let () = () in
  let a, b = clo () in
  a + b
