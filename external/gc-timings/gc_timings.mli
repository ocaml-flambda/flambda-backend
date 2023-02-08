val start_collection : unit -> unit

(* Time spend doing a minor collection in nanoseconds *)
val gc_minor_ns : unit -> float

(* Time spend doing a major collection in nanoseconds *)
val gc_major_ns : unit -> float
