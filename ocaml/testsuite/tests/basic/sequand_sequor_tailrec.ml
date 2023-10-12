(* TEST *)

(* Test that the right-hand sides of && and || are in tail position *)

let limit = 1000000

let rec f1 i x =
  if i > limit then x else x && f1 (i + 1) x

let rec f2 i x y z =
  if i > limit then x else x && y && z && f2 (i + 1) x y z

let rec f3 i x =
  if i > limit then x else x || f3 (i + 1) x

let rec f4 i x y z =
  if i > limit then x else x || y || z || f4 (i + 1) x y z

let () =
  ignore (Sys.opaque_identity (f1 0 true))

let () =
  ignore (Sys.opaque_identity (f2 0 true true true))

let () =
  ignore (Sys.opaque_identity (f3 0 false))

let () =
  ignore (Sys.opaque_identity (f4 0 false false false))
