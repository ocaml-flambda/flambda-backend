external nosimp : 'a -> 'a = "%obj_magic"

external ( + ) : int -> int -> int = "%addint"

external opaque : 'a -> 'a = "%opaque"

let test x =
  let pair = x, (x, x) in
  let impair = nosimp pair in
  let[@inline never] [@local never] ff (z, _) = z in
  let uu = nosimp (0, impair) in
  let vv = nosimp (0, (0, (0, 0))) in
  let w = ff (if opaque true then uu else vv) in
  let _, pepere = uu in
  let xx, (yy, zz) = pepere in
  xx + yy + zz + w
