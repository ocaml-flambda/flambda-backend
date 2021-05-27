open Memtrace.Trace

let print_duration ppf = function
  | n ->
     Printf.fprintf ppf "%.3f s" n

let print_bytes ppf = function
  | n when n < 1000. ->
     Printf.fprintf ppf "%3.0f B" n
  | n when n < 1000. *. 1024. ->
     let n = n /. 1024. in
     Printf.fprintf ppf "%3.*f kB" (if n < 10. then 1 else 0) n
  | n when n < 1000. *. 1024. *. 1024. ->
     let n = n /. 1024. /. 1024. in
     Printf.fprintf ppf "%3.*f MB" (if n < 10. then 1 else 0) n
  | n when n < 1000. *. 1024. *. 1024. *. 1024. ->
     let n = n /. 1024. /. 1024. /. 1024. in
     Printf.fprintf ppf "%3.*f GB" (if n < 10. then 1 else 0) n
  | n ->
     let n = n /. 1024. /. 1024. /. 1024. /. 1024. in
     Printf.fprintf ppf "%3.*f TB" (if n < 10. then 1 else 0) n

let identify filename =
  let trace = Reader.open_ ~filename in
  let info = Reader.info trace in
  Printf.printf "Trace file %s (%a)\n"
    filename print_bytes (Int64.to_float (Reader.size_bytes trace));
  let start_time = Timestamp.to_int64 info.start_time in
  let tm : Unix.tm = Unix.gmtime (Int64.to_float start_time *. 1e-6) in
  let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
  let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |] in
  Printf.printf "   of %s\n" info.executable_name;
  Printf.printf "   at %s %s %d %02d:%02d:%02d %d UTC\n"
    days.(tm.tm_wday) months.(tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec (tm.tm_year + 1900);
  Printf.printf "   on host %s (%d-bit), pid %Ld, sample rate %.1e\n"
    info.host_name info.word_size info.pid info.sample_rate;
  Printf.printf "   with OCaml GC params %s\n" info.ocaml_runtime_params;
  begin match info.context with
  | None -> ()
  | Some c -> Printf.printf "   with context: %s\n" c
  end;
  Printf.printf "Trace statistics:\n%!";
  let tmax = ref 0L in
  let minor_live = Obj_id.Tbl.create 100 in
  (* As an optimisation, only store allocations with nsamples > 1 *)
  let major_live_multisampled = Obj_id.Tbl.create 100 in
  let minor_alloc = ref 0 and minor_collect = ref 0 and promote = ref 0 in
  let major_alloc = ref 0 and major_collect = ref 0 in
  let dist4 = ref 0 and dist16 = ref 0 and dist256 = ref 0 and szmax = ref 0 in
  Reader.iter ~parse_backtraces:false trace (fun t ev ->
    tmax := Timedelta.to_int64 t;
    match ev with
    | Alloc {obj_id; length; nsamples; source; _} ->
       if source <> Minor then begin
         major_alloc := !major_alloc + nsamples;
         if nsamples > 1 then
           Obj_id.Tbl.add major_live_multisampled obj_id nsamples
       end else begin
         minor_alloc := !minor_alloc + nsamples;
         Obj_id.Tbl.add minor_live obj_id nsamples
       end;
       if length <= 4 then dist4 := !dist4 + nsamples;
       if length <= 16 then dist16 := !dist16 + nsamples;
       if length <= 256 then dist256 := !dist256 + nsamples;
       if length > !szmax then szmax := length
    | Promote id ->
       assert (Obj_id.Tbl.mem minor_live id);
       let nsamples = Obj_id.Tbl.find minor_live id in
       Obj_id.Tbl.remove minor_live id;
       promote := !promote + nsamples;
       if nsamples > 1 then
         Obj_id.Tbl.add major_live_multisampled id nsamples;
    | Collect id ->
       if Obj_id.Tbl.mem minor_live id then begin
         let nsamples = Obj_id.Tbl.find minor_live id in
         Obj_id.Tbl.remove minor_live id;
         minor_collect := !minor_collect + nsamples
       end else begin
         let nsamples =
           match Obj_id.Tbl.find major_live_multisampled id with
           | n -> Obj_id.Tbl.remove major_live_multisampled id; n
           | exception Not_found -> 1 in
         major_collect := !major_collect + nsamples
       end);
  let duration = Int64.to_float !tmax *. 1e-6 in
  let bytes nsamples =
    float_of_int nsamples /. info.sample_rate *. float_of_int (info.word_size / 8) in
  let minor_alloc = bytes !minor_alloc and _minor_collect = bytes !minor_collect and promote = bytes !promote in
  let major_alloc = bytes !major_alloc and major_collect = bytes !major_collect in
  let total_alloc = minor_alloc +. major_alloc in
  let dist4 = bytes !dist4 and dist16 = bytes !dist16 and dist256 = bytes !dist256 in
  Printf.printf "   Minor: %a/s allocations, %.1f%% promoted\n"
    print_bytes (minor_alloc /. duration)
    (100. *. promote /. minor_alloc);
  Printf.printf "   Major: %a/s allocations (%a/s direct), %a/s collections\n"
    print_bytes ((major_alloc +. promote) /. duration)
    print_bytes (major_alloc /. duration)
    print_bytes (major_collect /. duration);
  Printf.printf "   Sizes (by word): %.0f%% <= 4w, %.0f%% <= 16w, %.0f%% <= 256w. Max: %a\n"
    (100. *. dist4 /. total_alloc)
    (100. *. dist16 /. total_alloc)
    (100. *. dist256 /. total_alloc)
    print_bytes (float_of_int (!szmax * info.word_size / 8));
  Printf.printf "   Runtime: %a\n"
    print_duration duration;
  Reader.close trace


let () =
  if Array.length Sys.argv <> 2 then
    Printf.fprintf stderr "Usage: %s <trace file>\n" Sys.executable_name
  else
    identify Sys.argv.(1)
