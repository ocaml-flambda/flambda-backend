external collect_gc_timings : unit -> unit = "caml_timing_collect_gc" [@@noalloc]
external gc_minor_ns : unit -> float = "caml_timing_gc_time_spend_minor"
external gc_major_ns : unit -> float = "caml_timing_gc_time_spend_major"

let start_collection () =
  collect_gc_timings ()

let print ?(precision=3) ppf =
  let minor = gc_minor_ns () in
  let major = gc_major_ns () in
  let secs x = x *. 1e-9 in
  Format.fprintf ppf "%0.*fs gc\n" precision (secs (minor +. major));
  Format.fprintf ppf "  %0.*fs minor\n" precision (secs minor);
  Format.fprintf ppf "  %0.*fs major\n" precision (secs major)