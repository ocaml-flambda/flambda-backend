val start_collection : unit -> unit

(* Time spent doing a minor collection in nanoseconds *)
val gc_minor_ns : unit -> float

(* Time spent doing a major collection in nanoseconds *)
val gc_major_ns : unit -> float
