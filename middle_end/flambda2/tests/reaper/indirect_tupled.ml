external ( + ) : int -> int -> int = "%addint"

let test x =
  let[@inline never] [@local never] f (y, z) = x + y + z in
  let[@inline never] [@local never] g f = f (x, x) in
  g f
