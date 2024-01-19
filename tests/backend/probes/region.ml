(* minimal BUG example from alanechang *)
let _ =
  (fun f -> [%probe "" (
    let local_ a = "abc" in
    f a; ()
  )]) (fun x -> ())