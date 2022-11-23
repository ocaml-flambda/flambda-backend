(* The Failure exception raised by failwith is never used, so its symbol should
   disappear; however the Apply_cont corresponding to the raise must be
   rewritten to use a summy argument instead. This test creates a case where the
   rewriting needs to apply in a switch arm instead of a regular Apply_cont. *)

let[@inline never] opaque x = x

let n =
  try
    let[@local never] f () = if opaque true then 42 else failwith "urk" in
    f ()
  with _ -> 1
