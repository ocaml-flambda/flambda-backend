external nosimp : 'a -> 'a = "%obj_magic"

external ( + ) : int -> int -> int = "%addint"

external opaque : 'a -> 'a = "%opaque"

type 'a t = A of 'a * 'a t

let go () =
  let[@inline never] rec f x = A (x, f (x + 1)) in
  let[@inline never] rec consume (A (u, v)) = u + consume v in
  (* let A (x1, A (x2, _)) = f 0 in x1 + x2 *)
  consume (f 0)
