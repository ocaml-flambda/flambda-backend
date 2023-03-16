let[@inlined always] bar map_foo =
  fun () -> (map_foo [@inlined never]) ()

let rec map_foo () =
  bar map_foo ()
