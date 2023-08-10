external ( = ) : int -> int -> bool = "%equal"

external ( - ) : int -> int -> int = "%subint"

external ( mod ) : int -> int -> int = "%modint"

type parity =
  | Even
  | Odd

let rec even n = if n = 0 then true else odd (n - 1)

and odd n = if n = 0 then false else even (n - 1)

let three_is_even = (even [@unrolled 4]) 3

let four_is_odd = (odd [@unrolled 2]) 4
