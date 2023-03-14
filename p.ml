let f (x : unboxed_int64) =
  let () = Sys.opaque_identity () in
  let g (y : unboxed_int64) =
    let x' = (Int64.Unboxed.box x) in
    let y' = (Int64.Unboxed.box y) in
    let r = Int64.add x' y' in
    Int64.Unboxed.unbox r
  in
  let () = Sys.opaque_identity () in
  g
