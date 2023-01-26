external collect_gc_timings : unit -> unit = "caml_timing_collect_gc" [@@noalloc]
external gc_minor_ns : unit -> float = "caml_timing_gc_time_spend_minor"
external gc_major_ns : unit -> float = "caml_timing_gc_time_spend_major"

let start_collection () =
  collect_gc_timings ()

let print ?(precision=3) ppf =
  Format.fprintf ppf "%0.*fs gc_minor\n" precision (gc_minor_ns () *. 1e-9);
  Format.fprintf ppf "%0.*fs gc_major\n" precision (gc_major_ns () *. 1e-9)