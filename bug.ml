
let __dummy1__ _ = assert false[@@inline never ]
external __dummy2__ : unit -> 'a = "%opaque"
external __ignore__ : 'a -> unit = "%ignore"

external (||) : bool -> bool -> bool = "%sequor"
external (<) : int -> int -> bool = "%lessthan"

[@@@ocaml.flambda_o3]

let add_substring offset foo b =
  if (offset < 0) || b then ();

  if b then
    (if (foo (offset < 0); b)
     then ()
     else ())
