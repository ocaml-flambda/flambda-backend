let check_errors () =
  Unix.putenv "MEMTRACE" "/bad/file/name";
  (match Memtrace.trace_if_requested () with
   | _ -> assert false
   | exception (Unix.Unix_error _) -> ());
  Unix.putenv "MEMTRACE" "/tmp/goodfilename";
  (match Memtrace.trace_if_requested ~sampling_rate:(-3.) () with
   | _ -> assert false
   | exception (Invalid_argument _) -> ());
  Unix.putenv "MEMTRACE" "/tmp/goodfilename";
  Unix.putenv "MEMTRACE_RATE" "42";
  (match Memtrace.trace_if_requested () with
   | _ -> assert false
   | exception (Invalid_argument _) -> ());
  Unix.putenv "MEMTRACE" "/tmp/goodfilename";
  Unix.putenv "MEMTRACE_RATE" "potato";
  (match Memtrace.trace_if_requested () with
   | _ -> assert false
   | exception (Invalid_argument _) -> ())

let rec long_bt = function
  | 0 -> (Array.make 1000 0).(42)
  | n ->
    if Random.bool () then
      1 + long_bt (n-1)
    else
      2 + long_bt (n-1)

let go () =
  let filename = Filename.temp_file "memtrace" "ctf" in
  let r = ref [| |] in
  let t = Memtrace.start_tracing ~context:(Some "ctx") ~sampling_rate:0.1 ~filename in
  r := Array.make 4242 42;
  for _i = 1 to 10 do
    let n = long_bt 10_000 in
    assert (n > 0);
  done;
  for _i = 1 to 1000 do
    Option.iter Memtrace.External.free
      (Memtrace.External.alloc ~bytes:((Sys.word_size / 8) * 7))
  done;
  Memtrace.stop_tracing t;
  let r = Memtrace.Trace.Reader.open_ ~filename in
  let first = ref true in
  let n_long = ref 0 in
  let last_ext = ref None in
  let ext_samples = ref 0 in
  Memtrace.Trace.Reader.iter r (fun _ ev ->
    match ev with
    | Alloc info when !first ->
      first := false;
      assert (info.length = 4242);
      ()
    | Alloc info when info.length = 1000 ->
      (* backtraces should be truncated *)
      assert (info.backtrace_length > 3500 && info.backtrace_length < 4000);
      incr n_long
    | Alloc info when info.length = 7 ->
      last_ext := Some info.obj_id;
      ext_samples := !ext_samples + info.nsamples;
    | Collect id ->
      assert (!last_ext = Some id);
      last_ext := None
    | e ->
      failwith ("unexpected " ^ (Memtrace.Trace.Event.to_string
                                   (Memtrace.Trace.Reader.lookup_location_code r) e)));
  Memtrace.Trace.Reader.close r;
  Unix.unlink filename;
  assert (650 <= !ext_samples && !ext_samples < 750);
  assert (not !first);
  assert (!n_long = 10)


let () =
  go (); go ()
