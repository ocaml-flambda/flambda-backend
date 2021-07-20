external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external array_get: 'a array -> int -> 'a = "%array_safe_get"
external array_set: 'a array -> int -> 'a -> unit = "%array_safe_set"

let n =
  let rec f x =
    if x > 0 then 1 + f (x - 1)
    else 42
  in
  (f [@unrolled 10]) 3
