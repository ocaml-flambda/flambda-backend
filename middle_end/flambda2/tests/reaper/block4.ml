external nosimp : 'a -> 'a = "%obj_magic"

external ( + ) : int -> int -> int = "%addint"

external opaque : 'a -> 'a = "%opaque"

let test x =
  let[@local] uu (_, a) () = a in
  let[@local] vv (a, _) () = a in
  let y = nosimp (x, x) in
  let z = nosimp (x, x) in
  if opaque false
  then if opaque false then uu y () else uu z ()
  else if opaque false
  then vv y ()
  else vv z ()
