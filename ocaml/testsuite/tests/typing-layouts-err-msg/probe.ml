(* TEST
   flags = "-extension layouts_alpha"
   compiler_reference = "${test_source_directory}/probe.reference"
* native

*)
let f (x: float#) = [%probe "a" (
  let f () = x in
  ()
)]
