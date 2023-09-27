external ( = ) : int -> int -> bool = "%eq"

external ( - ) : int -> int -> int = "%subint"

external ( mod ) : int -> int -> int = "%modint"

type parity =
  | Even
  | Odd

let[@inline always] parity_is p n ~k =
  let rec even n = if n = 0 then (k [@inlined hint]) true else odd (n - 1)
  and odd n = if n = 0 then (k [@inlined hint]) false else even (n - 1) in
  match p with Even -> (even [@unrolled 3]) n | Odd -> (odd [@unrolled 4]) n

let[@inline always] k b = b

let one_is_even = parity_is Even 1 ~k

let four_is_odd = parity_is Odd 4 ~k
