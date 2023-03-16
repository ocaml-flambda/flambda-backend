let bar foo =
  fun () -> foo ()

let rec foo () =
  let g = bar foo in
  g ()
