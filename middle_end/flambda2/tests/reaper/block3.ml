external nosimp : 'a -> 'a = "%obj_magic"

external ( + ) : int -> int -> int = "%addint"

external opaque : 'a -> 'a = "%opaque"

let test x =
  let p1 = x, x in
  let p2 = p1, p1 in
  let p3 = p2, p2 in
  let p4 = p3, p3 in
  let p3a, p3b = nosimp p4 in
  let p2a, p2b = if opaque false then p3a else p3b in
  let p1a, p1b = if opaque false then p2a else p2b in
  let xa, xb = if opaque false then p1a else p1b in
  xa + xb
