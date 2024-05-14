let[@zero_alloc strict][@inline never] foo n m = n + m
let[@zero_alloc][@inline never] baz n =
  if n > 0 then
    n * 3
  else
    let s = String.concat " " ["baz"; "with"; string_of_int n] in
    raise (Failure s)
