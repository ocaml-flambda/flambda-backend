external opaque : 'a -> 'a = "%opaque"
external ( + ) : int -> int -> int = "%addint"

let x = opaque 1

let f y = x + y
