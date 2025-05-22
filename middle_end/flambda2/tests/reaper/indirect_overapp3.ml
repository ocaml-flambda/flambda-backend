external ( + ) : int -> int -> int = "%addint"

let test x y z b =
  let xx = x, x in
  let[@inline always] sump (x, y) = x + y in
  let[@inline never] [@local never] f y = sump xx + sump y in
  let[@inline never] [@local never] id1 f = f in
  let[@inline never] [@local never] id2 f = f in
  (if b then id1 else id2) f (y, y)
