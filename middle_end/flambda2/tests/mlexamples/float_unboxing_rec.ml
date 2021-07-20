
let f n x =
  let r = ref x in
  for i = 0 to n do
    r := !r +. x
  done;
  !r +. 0.

