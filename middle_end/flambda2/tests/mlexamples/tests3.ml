external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external array_get: 'a array -> int -> 'a = "%array_safe_get"
external array_set: 'a array -> int -> 'a -> unit = "%array_safe_set"

let foo arr f i =
  array_set arr i (f (array_get arr i))

let f c m n x' y' =
  let x = if c < 0 then x' else x' + 10 in
  let y = if c < 0 then y' else y' + 20 in
  x + y
