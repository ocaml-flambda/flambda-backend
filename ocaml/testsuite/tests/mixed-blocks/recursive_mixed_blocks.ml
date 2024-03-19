(* TEST *)

type t = { t : t; flt : float# }

(* Run Gc.full_major while constructing [t] so that it is promoted
   to the major heap. This ensures that the [caml_modify] run as part
   of [caml_update_dummy] actually does some interesting work.
*)

let rec t =
  { t;
    flt = (Gc.full_major (); #0.);
  };;

let () = Gc.full_major ()

let (_ : t) = Sys.opaque_identity t
