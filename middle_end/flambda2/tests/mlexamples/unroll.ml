external ( = ) : int -> int -> bool = "%equal"

external ( - ) : int -> int -> int = "%subint"

external ( * ) : int -> int -> int = "%mulint"

let rec fact n = if n = 0 then 1 else n * fact (n - 1)

let i = (fact [@unrolled 4]) 6

let j = (fact [@unrolled 7]) 6
