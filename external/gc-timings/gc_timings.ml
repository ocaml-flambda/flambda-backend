external collect_gc_timings : unit -> unit = "caml_timing_collect_gc" [@@noalloc]
external gc_minor_ns : unit -> float = "caml_timing_gc_time_spent_minor"
external gc_major_ns : unit -> float = "caml_timing_gc_time_spent_major"

let start_collection () =
  collect_gc_timings ()
