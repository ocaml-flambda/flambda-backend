[@@@ocaml.flambda_o3]

external opaque_identity : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"
let[@inline never] ignore_local (local_ x) = let _ = opaque_identity x in ()

let allocate_in_current_region x = exclave_
  ignore_local (Some x);
  ()

external make_dumb_external_block : unit -> int = "make_dumb_external_block"
external follow : int -> ('a [@local_opt]) = "%int_as_pointer"

let ext : int = make_dumb_external_block ()

let[@inline never] int_as_pointer_local x =
  let leak_in_current_region : 'a -> unit = Obj.magic allocate_in_current_region in
  leak_in_current_region x;
  let _ = opaque_identity (follow ext) in
  ()
