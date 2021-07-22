external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( < ) : 'a -> 'a -> bool = "%lessthan"

external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"

external ( == ) : 'a -> 'a -> bool = "%eq"

let pr2162_3_as_int64 z x =
  let y =
    if z then x * 2
    else x
  in
  y == x
