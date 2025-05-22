type void : void
external void : unit -> void = "%unbox_unit"

let foo () =
  if Sys.opaque_identity true then void ()
  else (
    (Printf.printf [@inlined never]) "should never print\n%!";
    void ()
  )

let () =
  let _ = foo () in
  ()
