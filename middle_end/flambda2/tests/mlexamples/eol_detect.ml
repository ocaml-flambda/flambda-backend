type 'a ref = { mutable contents : 'a }

external ref : 'a -> 'a ref = "%makemutable"

external ( ! ) : 'a ref -> 'a = "%field0"

external ( := ) : 'a ref -> 'a -> unit = "%setfield0"

external ( <> ) : 'a -> 'a -> bool = "%notequal"

let f ~read x =
  let c = ref ' ' in
  try
    while
      c := read ();
      !c <> '\n'
    do
      ()
    done;
    match x with None -> 0 | Some 'x' -> 1 | Some _ -> 0
  with _ -> 0
