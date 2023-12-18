(* TEST

   flags = "-extension layouts_alpha"
*)

type t = { t : t; float : float# }

let rec t = { t; float = #4.0 };;

let () = Gc.full_major ()

let (_ : t) = Sys.opaque_identity t
