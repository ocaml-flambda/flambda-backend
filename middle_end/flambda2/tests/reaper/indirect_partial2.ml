external ( + ) : int -> int -> int = "%addint"

let test x b =
  let x = x, x in
  let[@inline always] sump (x, y) = x + y in
  let[@inline never] [@local never] f y z = sump x + sump y + sump z in
  let[@inline never] [@local never] g f = f x in
  let u = g f in
  (if b then sump else u) x
