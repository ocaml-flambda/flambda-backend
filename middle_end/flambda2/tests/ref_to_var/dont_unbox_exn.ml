exception FoundAt of int

external opaque : 'a -> 'a = "%opaque"

external raise : exn -> 'a = "%raise"

let test () =
  try
    if opaque false then raise (FoundAt (if opaque false then 1 else 2));
    0
  with FoundAt i -> i
