external external_test : unit -> unit = "test"

let[@zero_alloc] test7 x =
  external_test ()
