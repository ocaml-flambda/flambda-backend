type 'a ref = { mutable contents : 'a }

external ref : 'a -> ('a ref[@local_opt]) = "%makemutable"

external ( ! ) : ('a ref[@local_opt]) -> 'a = "%field0"

external ( := ) : ('a ref[@local_opt]) -> 'a -> unit = "%setfield0"

external incr : (int ref[@local_opt]) -> unit = "%incr"

external decr : (int ref[@local_opt]) -> unit = "%decr"

external ( +. ) :
  (float[@local_opt]) -> (float[@local_opt]) -> (float[@local_opt])
  = "%addfloat"

let f x =
  let r = ref x in
  let g = ref 0. in
  for i = 0 to 10 do
    g := !g +. !r
  done;
  !g
