let bar foo () = foo ()

let foo () =
  let g = bar Sys.opaque_identity in
  g ()
