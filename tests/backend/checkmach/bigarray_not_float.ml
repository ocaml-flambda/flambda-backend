open Bigarray

let[@zero_alloc]
  f (x : (float, float64_elt, c_layout) Bigarray.Array1.t array) = x.(1)
