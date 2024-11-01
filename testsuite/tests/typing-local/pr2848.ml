(* TEST *)

let f (x : int) y =
  let[@local always] h (y : int) =
    let _ = local_ (x, y) in
    if y < 42 then failwith "foo"
  in
  let _m = ref 42 in
  let l = local_ (x, x) in
  h (fst l)
