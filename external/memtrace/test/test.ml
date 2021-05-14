open Memtrace.Trace

let with_temp f =
  let s = Filename.temp_file "memtrace" "ctf" in
  let fd = Unix.openfile s [O_RDWR] 0o600 in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists s then Unix.unlink s)
    (fun () -> f fd)

let mkloc filename line start_char end_char defname =
  Location.{ filename; line; start_char; end_char; defname }

(* These locations are expected to be encoded and decoded accurately *)
let reasonable_locations =
  [ [mkloc "foo.ml" 42 100 120 "func"];
    [mkloc "apiosdjfoaijsdf.ml" 100 58 1023 "aiosjdf"];
    [mkloc "apiosdjfoaijsdf.ml" 19 97 1023 "aiosjdf"; mkloc "inline" 1 1 1 "fjkisda"]]

(* These locations are too big, and are expected to be encoded as "unknown" *)
let ridiculous_locations =
  [ [mkloc (String.make 60000 'j') 1000 1000 1000 (String.make 60000 'a')];
    List.init 10000 (fun _ -> mkloc "asdf.ml" 42 93 84 "Asdf.fun")]

let locations = Array.of_list (reasonable_locations @ ridiculous_locations)

let id : int -> Obj_id.t = Obj.magic

let loc (n : int) : Location_code.t = Obj.magic (n+1)

let info : Info.t = {
  sample_rate = 0.01;
  word_size = 64;
  executable_name = "exec";
  host_name = "host";
  ocaml_runtime_params = "runtime";
  pid = 42L;
  start_time = Timestamp.of_int64 23897423L;
  context = Some "context"
}

let events : (int * Event.t) list =
  let big_length =
    if Int.max_int > 0x3fff_ffff
      then int_of_string "0x81234567"
      else 0x31234567
  in
  [ 0, Alloc {obj_id = id 0;
              length = 42;
              nsamples = 1;
              source = Minor;
              backtrace_buffer = [| loc 0; loc 1 |];
              backtrace_length = 2;
              common_prefix = 0};
    1, Alloc {obj_id = id 1;
              length = 2;
              nsamples = 1;
              source = Major;
              backtrace_buffer = [| loc 0; loc 1; loc 2 |];
              backtrace_length = 3;
              common_prefix = 2};
    100, Alloc {obj_id = id 2;
              length = big_length;
              nsamples = 1;
              source = Minor;
              backtrace_buffer = [| loc 0; loc 1; loc 2 |];
              backtrace_length = 3;
              common_prefix = 3};
    101, Collect (id 1);
    102, Promote (id 2);
    103, Alloc {obj_id = id 3;
                length = 2;
                nsamples = 1;
                source = External;
                backtrace_buffer = [| loc 3 |];
                backtrace_length = 1;
                common_prefix = 0};
    103, Alloc {obj_id = id 4;
                length = 2;
                nsamples = 1;
                source = Minor;
                backtrace_buffer = [| loc 4 |];
                backtrace_length = 1;
                common_prefix = 0}
  ]

let copy_event : Event.t -> Event.t = function
  | Alloc ev ->
     Alloc { ev with backtrace_buffer =
       Array.sub ev.backtrace_buffer 0 ev.backtrace_length  }
  | ev -> ev

let test () = with_temp @@ fun fd ->
  let w = Writer.create fd info in
  let decode_loc l = locations.((l : Location_code.t :> int) - 1) in
  events |> List.iter (fun (i, ev) ->
     let now = Int64.(add (Timestamp.to_int64 info.start_time) (of_int (i * 1_000_000))) in
     Writer.put_event w
       ~decode_callstack_entry:decode_loc
       (Timestamp.of_int64 now) ev);
  Writer.flush w;
  let _ : int = Unix.lseek fd 0 SEEK_SET in
  let r = Reader.create fd in
  assert (Reader.info r = info);
  let evs = ref [] in
  Reader.iter r ~parse_backtraces:true (fun td ev ->
    (* assert (Int64.rem td 1_000_000L = 0L); *)
    evs := ((Int64.(to_int (div (Timedelta.to_int64 td) 1_000_000L)), copy_event ev) :: !evs));
  let rec compare exp act =
    match exp, act with
    | [], [] -> ()
    | exp :: exps, act :: acts when exp = act ->
       compare exps acts
    | [], (tact, act) :: _ ->
       Printf.printf "Extra event decoded:\n  %d. %s\n%!"
         tact
         (Event.to_string decode_loc act);
       failwith "Extra events"
    | (texp, exp) :: _, [] ->
       Printf.printf "Missing event:\n  %d. %s\n%!"
         texp
         (Event.to_string decode_loc exp);
       failwith "Missing events"
    | (texp, exp) :: _, (tact, act) :: _ ->
       Printf.printf "Event doesn't match. Expected:\n  %d. %s\nbut decoded:\n  %d. %s\n%!"
         texp (Event.to_string decode_loc exp)
         tact (Event.to_string decode_loc act);
       failwith "Incorrect event"
  in
  compare events (List.rev !evs);
  for i = 0 to List.length reasonable_locations - 1 do
    assert (Reader.lookup_location_code r (loc i) = locations.(i))
  done;
  for i = List.length reasonable_locations to Array.length locations - 1 do
    assert (Reader.lookup_location_code r (loc i) = [Location.unknown])
  done;
  Reader.close r

let test_failure () = with_temp @@ fun fd ->
  let w = Writer.create fd info in
  let r = ref [] in
  let report_exn e = r := e :: !r in
  let t = Memtrace.Memprof_tracer.start ~report_exn ~sampling_rate:0.2 w in
  for _i = 1 to 50000 do
    let _ : int ref = Sys.opaque_identity (ref 42) in
    ()
  done;
  let rd, wr = Unix.pipe () in
  if not Sys.win32 then
    Sys.set_signal Sys.sigpipe Signal_ignore;
  Unix.close rd;
  Unix.dup2 wr fd;
  Unix.close wr;
  for _i = 1 to 50000 do
    let _ : int ref = Sys.opaque_identity (ref 42) in
    ()
  done;
  Memtrace.Memprof_tracer.stop t;
  match !r with
  | [Unix.Unix_error(Unix.EPIPE, "write", _)] -> ()
  | [] -> failwith "should have failed"
  | e :: _ -> raise e

let () = test ()
let () = test_failure ()
