external nosimp : 'a -> 'a = "%obj_magic"

external ( + ) : int -> int -> int = "%addint"

external opaque : 'a -> 'a = "%opaque"

let test a b =
  let[@inline never] [@local never] mkalloc u v = u, v in
  let z1 = nosimp (mkalloc a a) in
  let z2 = nosimp (mkalloc b b) in
  let uz = nosimp (z1, z2) in
  let w1, w2 = uz in
  let i, j = w1 in
  let k, l = w2 in
  i + j + k + l
