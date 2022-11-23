type 'a ref = { mutable contents : 'a }

external ref : 'a -> 'a ref = "%makemutable"

external ( ! ) : 'a ref -> 'a = "%field0"

external ( := ) : 'a ref -> 'a -> unit = "%setfield0"

external magic : 'a -> 'b = "%identity"

external opaque_identity : 'a -> 'a = "%opaque"

let[@inline never] read_int () = 42

let gasp_in_horror () =
  let r = ref (magic 0.0) in
  if opaque_identity true then r := read_int ();
  !r
