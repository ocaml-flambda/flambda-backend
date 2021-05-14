open Memtrace.Trace
let dump filename =
  let trace = Reader.open_ ~filename in
  Reader.iter trace (fun time ev ->
    Printf.printf "%010Ld " (Timedelta.to_int64 time);
    match ev with
  | Alloc {obj_id; length; nsamples; source; backtrace_buffer; backtrace_length; common_prefix} ->
    let src =
      match source with
      | Minor -> "alloc"
      | Major -> "alloc_major"
      | External -> "alloc_ext" in
    Printf.printf "%010d %s %d len=%d % 4d:" (obj_id :> int) src nsamples length common_prefix;
    let print_location ppf loc =
      Printf.fprintf ppf "%s" (Location.to_string loc) in
    for i = 0 to backtrace_length - 1 do
      let s = backtrace_buffer.(i) in
      match Reader.lookup_location_code trace s with
      | [] -> Printf.printf " $%d" (s :> int)
      | ls -> ls |> List.iter (Printf.printf " %a" print_location)
    done;
    Printf.printf "\n%!"
  | Promote id ->
    Printf.printf "%010d promote\n" (id :> int)
  | Collect id ->
    Printf.printf "%010d collect\n" (id :> int));
  Reader.close trace


let () =
  if Array.length Sys.argv <> 2 then
    Printf.fprintf stderr "Usage: %s <trace file>\n" Sys.executable_name
  else
    dump Sys.argv.(1)
