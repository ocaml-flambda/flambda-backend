open Memtrace.Trace

let copy inf outf =
  let r = Reader.open_ ~filename:inf in
  let wfd = Unix.openfile outf [O_CREAT;O_WRONLY;O_TRUNC] 0o600 in
  let info = Reader.info r in
  let w = Writer.create wfd ~getpid:(fun () -> info.pid) info in
  Reader.iter r (fun now ev ->
    Writer.put_event w
      ~decode_callstack_entry:(fun loc ->
        Reader.lookup_location_code r loc)
      (Timedelta.offset info.start_time now) ev);
  Reader.close r;
  Writer.flush w;
  Unix.close wfd

let () =
  match Sys.argv with
  | [| _; inf; outf |] ->
     copy inf outf
  | _ -> Printf.fprintf stderr "usage: copy <in> <out>\n%!"; exit 1
