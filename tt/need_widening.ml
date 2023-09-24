type t = A of t * int

let f x =
  let r = ref x in
  while Sys.opaque_identity true do
    let A (t, _) = !r in
    r := t;
  done;
  let A (_, x) = !r in
  x
