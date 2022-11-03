let id = Sys.opaque_identity

let f x =
  let r = ref x in
  while id false do
    let _ = id !r in
    ()
  done;
  !r + 1
