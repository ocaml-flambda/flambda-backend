type rosetta2_status =
  | Unknown
  | Off
  | On

val get_rosetta2_status : unit -> rosetta2_status

val run_if_not_under_rosetta2 : f:(unit -> unit) -> unit
