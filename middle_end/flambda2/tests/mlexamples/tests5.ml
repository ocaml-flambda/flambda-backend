external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( < ) : 'a -> 'a -> bool = "%lessthan"

(*
let rec length_aux len = function
    [] -> len
  | _::l -> length_aux (len + 1) l
*)

let rec f x =
  if x < 4 then g (x - 1)
  else 42

and g y =
  if y > 3 then f (y - 1)
  else 7
