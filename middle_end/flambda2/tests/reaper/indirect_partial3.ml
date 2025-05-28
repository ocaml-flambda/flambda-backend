external ( + ) : int -> int -> int = "%addint"

let test x y z b =
  let xx = x, x in
  let[@inline always] sump (x, y) = x + y in
  let[@inline never] [@local never] f y z w =
    sump xx + sump y + sump z + sump w
  in
  let[@inline never] [@local never] g f = f (x, x) in
  let u = g f in
  let yy = y, y in
  let zz = z, z in
  let _ = (if b then fun (a, b) (c, d) -> a + b + c + d else u) yy in
  u yy zz
