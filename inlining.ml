external ( + ) : int -> int -> int = "%addint"
external opaque_identity : 'a -> 'a = "%opaque"

let[@inline] f x = x + 1

let[@inline never] foo a =
  let x1 = a + 42 in
  let x1 = opaque_identity x1 in
  let x2 = f a in
  let _x2 = opaque_identity x2 in
  let x1 = opaque_identity x1 in
  let x2 = f a in
  let x2 = opaque_identity x2 in
  let x3 = a + 100 in
  x1, x2 + x3

let () =
  let _, _ = opaque_identity (foo 42) in
  ()
