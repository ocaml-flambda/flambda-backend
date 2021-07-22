external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external opaque : 'a -> 'a = "%opaque"

let z = opaque 42

let rec f x =
  z + g (x + 1)
and g y =
  z + f (y - 1)
