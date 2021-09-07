let foobar b x z =
  let f =
    if b
    then
      let[@inline] bar1 y = x + y in
      Ok bar1
    else
      let[@inline] bar2 y = z * x * y in
      Error bar2
  in
  match f with Ok f -> f x | Error f -> f x
