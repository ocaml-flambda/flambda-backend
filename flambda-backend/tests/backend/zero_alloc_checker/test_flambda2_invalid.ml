external opaque : 'a -> 'a = "%opaque"
external box : float# -> (float[@local_opt]) = "%box_float"
external unbox : (float[@local_opt]) -> float# = "%unbox_float"

let f _ = assert false

let _test () =
  let a = opaque 0.0 |> unbox in
  List.iter
    (fun [@zero_alloc] _ ->
       let _ : float = box a +. 0.0 |> opaque in
       f #0.0) []
