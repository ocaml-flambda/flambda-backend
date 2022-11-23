let f x =
  let a = Array.make 42 0 in
  Array.unsafe_set a 0 x;
  a
