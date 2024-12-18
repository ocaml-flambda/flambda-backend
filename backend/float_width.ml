type t =
  | Float64
  | Float32

let[@inline hint] equal (t1 : t) (t2 : t) = t1 = t2
