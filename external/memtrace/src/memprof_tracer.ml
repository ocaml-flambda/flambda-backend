module Thread = struct
  let yield () : unit = failwith "only single threaded programs supported"
end

type t =
  { mutable locked : bool;
    mutable locked_ext : bool;
    mutable failed : bool;
    mutable stopped : bool;
    report_exn : exn -> unit;
    trace : Trace.Writer.t;
    ext_sampler : Geometric_sampler.t; }

let curr_active_tracer : t option ref = ref None

let active_tracer () = !curr_active_tracer

let bytes_before_ext_sample = ref max_int

let draw_sampler_bytes t =
  Geometric_sampler.draw t.ext_sampler * (Sys.word_size / 8)

let[@inline never] rec lock_tracer s =
  if s.locked then
    if s.locked_ext then false
    else (Thread.yield (); lock_tracer s)
  else if s.failed then
    false
  else
    (s.locked <- true; true)

let[@inline never] rec lock_tracer_ext s =
  if s.locked then
    (Thread.yield (); lock_tracer_ext s)
  else if s.failed then
    false
  else
    (s.locked <- true; s.locked_ext <- true; true)

let[@inline never] unlock_tracer s =
  assert (s.locked && not s.locked_ext && not s.failed);
  s.locked <- false

let[@inline never] unlock_tracer_ext s =
  assert (s.locked && s.locked_ext && not s.failed);
  s.locked_ext <- false;
  s.locked <- false

let[@inline never] mark_failed s e =
  assert (s.locked && not s.failed);
  s.failed <- true;
  s.locked <- false;
  s.locked_ext <- false;
  s.report_exn e

let default_report_exn e =
  let msg = Printf.sprintf "Memtrace failure: %s\n" (Printexc.to_string e) in
  output_string stderr msg;
  Printexc.print_backtrace stderr;
  flush stderr

let start ?(report_exn=default_report_exn) ~sampling_rate trace =
  let ext_sampler = Geometric_sampler.make ~sampling_rate () in
  let s = { trace; locked = false; locked_ext = false; stopped = false; failed = false;
            report_exn; ext_sampler } in
  let tracker : (_,_) Gc.Memprof.tracker = {
    alloc_minor = (fun info ->
      if lock_tracer s then begin
        match Trace.Writer.put_alloc_with_raw_backtrace trace (Trace.Timestamp.now ())
                ~length:info.size
                ~nsamples:info.n_samples
                ~source:Minor
                ~callstack:info.callstack
        with
        | r -> unlock_tracer s; Some r
        | exception e -> mark_failed s e; None
      end else None);
    alloc_major = (fun info ->
      if lock_tracer s then begin
        match Trace.Writer.put_alloc_with_raw_backtrace trace (Trace.Timestamp.now ())
                ~length:info.size
                ~nsamples:info.n_samples
                ~source:Major
                ~callstack:info.callstack
        with
        | r -> unlock_tracer s; Some r
        | exception e -> mark_failed s e; None
      end else None);
    promote = (fun id ->
      if lock_tracer s then
        match Trace.Writer.put_promote trace (Trace.Timestamp.now ()) id with
        | () -> unlock_tracer s; Some id
        | exception e -> mark_failed s e; None
      else None);
    dealloc_minor = (fun id ->
      if lock_tracer s then
        match Trace.Writer.put_collect trace (Trace.Timestamp.now ()) id with
        | () -> unlock_tracer s
        | exception e -> mark_failed s e);
    dealloc_major = (fun id ->
      if lock_tracer s then
        match Trace.Writer.put_collect trace (Trace.Timestamp.now ()) id with
        | () -> unlock_tracer s
        | exception e -> mark_failed s e) } in
  curr_active_tracer := Some s;
  bytes_before_ext_sample := draw_sampler_bytes s;
  let _ = Gc.Memprof.start
    ~sampling_rate
    ~callstack_size:max_int
    tracker in
  s

let stop s =
  if not s.stopped then begin
    s.stopped <- true;
    Gc.Memprof.stop ();
    if lock_tracer s then
      Trace.Writer.close s.trace;
    curr_active_tracer := None
  end

let[@inline never] ext_alloc_slowpath ~bytes =
  match !curr_active_tracer with
  | None -> bytes_before_ext_sample := max_int; None
  | Some s ->
    if lock_tracer_ext s then begin
      match
        let bytes_per_word = Sys.word_size / 8 in
        (* round up to an integer number of words *)
        let size_words = (bytes + bytes_per_word - 1) / bytes_per_word in
        let samples = ref 0 in
        while !bytes_before_ext_sample <= 0 do
          bytes_before_ext_sample :=
            !bytes_before_ext_sample + draw_sampler_bytes s;
          incr samples
        done;
        assert (!samples > 0);
        let callstack = Printexc.get_callstack max_int in
        Some (Trace.Writer.put_alloc_with_raw_backtrace s.trace
                (Trace.Timestamp.now ())
                ~length:size_words
                ~nsamples:!samples
                ~source:External
                ~callstack)
      with
      | r -> unlock_tracer_ext s; r
      | exception e -> mark_failed s e; None
    end else None


type ext_token = Trace.Obj_id.t

let ext_alloc ~bytes =
  let n = !bytes_before_ext_sample - bytes in
  bytes_before_ext_sample := n;
  if n <= 0 then ext_alloc_slowpath ~bytes else None

let ext_free id =
  match !curr_active_tracer with
  | None -> ()
  | Some s ->
    if lock_tracer_ext s then begin
      match
        Trace.Writer.put_collect s.trace (Trace.Timestamp.now ()) id
      with
      | () -> unlock_tracer_ext s; ()
      | exception e -> mark_failed s e; ()
    end
