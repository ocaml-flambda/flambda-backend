type 'a ref = { mutable contents : 'a; }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( + ) : int -> int -> int = "%addint"

let r = ref 42

let f x = x + !r

let g x = f x + !r

