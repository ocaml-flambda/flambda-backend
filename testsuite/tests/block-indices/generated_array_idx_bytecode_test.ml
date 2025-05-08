
type t112 = { mutable a112 : int64# }

(* Test record idx get and set *)
let () =
  (*************************)
  (*   t112 = { int64# }   *)
  let _ = ((.a112)) in
  ()
