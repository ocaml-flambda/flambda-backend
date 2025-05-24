external nosimp : 'a -> 'a = "%obj_magic"

external ( + ) : int -> int -> int = "%addint"

external opaque : 'a -> 'a = "%opaque"

let test x =
  let pair = x, (x, x) in
  let impair = nosimp pair in
  (* let[@inline never][@local never] ff (z, _) = z in let w = ff (if opaque
     true then impair else (0, (0, 0))) in *)
  let xx, (yy, zz) = impair in
  xx + yy + zz (* + w *)
