external ( < ) : int -> int -> bool = "%lessthan"

external opaque : 'a -> 'a = "%opaque"

let f x z =
  let[@inline] test (_, y) = if y < z then opaque 0 else opaque 0 in
  let _ = test (0, x) in
  (* will correctly be unboxed if you delete the above line *)
  test (if opaque true then 0, x else 1, x)
