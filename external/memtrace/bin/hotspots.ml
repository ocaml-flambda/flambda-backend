open Memtrace.Trace

module Heavy_hitters (A : Hashtbl.HashedType) : sig
  type t
  val make : int -> t
  val add : t -> A.t -> unit
  val length : t -> int
  val iter : t -> (A.t -> int -> int -> unit) -> unit
end = struct
  module Tbl = Hashtbl.Make(A)

  type counter = {
    (* (hits - misses) is the usual Misra-Gries summary *)
    mutable hits : int;
    mutable misses : int;
    (* number of elements added before this one (including duplicates) *)
    added_before : int;
    (* number of elements skipped before this one was added *)
    skipped_before : int;
  }

  (*let upper_bound t c =
    c.skipped_before
   *)
  type t = {
    k : int;
    tbl : counter Tbl.t;
    mutable len : int;
    mutable added : int;
    mutable skipped : int;
  }

  let make k =
    {
      k;
      tbl = Tbl.create k;
      len = 0;
      added = 0;
      skipped = 0;
    }

  let _check t =
    let hits = ref 0 in
    t.tbl |> Tbl.iter (fun _k c -> hits := !hits + c.hits);
    assert (t.len = Tbl.length t.tbl);
    assert (!hits + t.skipped = t.added);
    ()

  let add t x =
  (*  check t;*)
    begin match Tbl.find_opt t.tbl x with
    | Some c ->
       c.hits <- c.hits + 1
    | None ->
       if t.len < t.k then begin
         t.len <- t.len + 1;
         Tbl.add t.tbl x { hits = 1; misses = 0; added_before = t.added; skipped_before = t.skipped }
       end else begin
         t.skipped <- t.skipped + 1;
         t.tbl |> Tbl.filter_map_inplace (fun _k c ->
           c.misses <- c.misses + 1;
           if c.hits > c.misses then Some c else begin
             t.len <- t.len - 1;
             t.skipped <- t.skipped + c.hits;
             None
           end)
       end
    end;
    t.added <- t.added + 1

  let length t = t.added

  let iter t f =
    t.tbl |> Tbl.iter (fun x c -> f x c.hits c.skipped_before)

end



module Loc_tbl = Hashtbl.Make (struct
  type t = Location_code.t
  let hash (x : Location_code.t) = ((x :> int) * 218854569) lsr 17
  let equal (x : Location_code.t) (y : Location_code.t) = (x = y)
end)

module Str_tbl = Hashtbl.Make (struct
  type t = string
  let hash = Hashtbl.hash
  let equal = String.equal
end)

type loc_entry = {
  line: int;
  start_ch: int;
  end_ch: int;
  func: func;
  mutable alloc_count: int;
}
and func = {
  id : int;
  name : string;
  filename : string;
  mutable locs : loc_entry list;
  mutable total_count : int;

  mutable n_allocs : int;
  mutable total_dist_to_alloc : int;
}

module Func_tbl = Hashtbl.Make (struct
  type t = func
  let hash (f : func) = f.id * 21089245
  let equal (f : func) (g : func) = f.id = g.id
end)

let total_allocs (f : func) =
  List.fold_left (fun k e -> k + e.alloc_count) f.total_count f.locs

let direct_allocs (f : func) =
  List.fold_left (fun k e -> k + e.alloc_count) 0 f.locs

let avg_dist_to_alloc (f : func) =
  float_of_int f.total_dist_to_alloc /. float_of_int f.n_allocs

type loc_table = {
  entries : loc_entry Loc_tbl.t;
  funcs : (string * string, func) Hashtbl.t;
  trace : Reader.t;
  mutable next_id : int;
}

let new_loc_table trace =
  { entries = Loc_tbl.create 10000;
    funcs = Hashtbl.create 10000;
    trace;
    next_id = 0 }

let rec describe_location ?(max_discard=2) trace buf i =
  match Reader.lookup_location_code trace buf.(i) with
  | [] when i > 0 && max_discard > 0 ->
    describe_location ~max_discard:(max_discard - 1) trace buf (i-1)
  | [] -> "??", Printf.sprintf "#%x" (buf.(i) :> int), 0, 0, 0
  | locs ->
    let l = (List.nth locs (List.length locs - 1)) in
    l.filename, l.defname, l.line, l.start_char, l.end_char

let add_loc t buf i =
  let loc = buf.(i) in
  match Loc_tbl.find t.entries loc with
  | e -> e
  | exception Not_found ->
     let filename, funcname, line, start_ch, end_ch =
       describe_location t.trace buf i in
     let func =
       match Hashtbl.find t.funcs (filename, funcname) with
       | func ->
          func
       | exception Not_found ->
          let id = t.next_id in
          t.next_id <- t.next_id + 1;
          let func : func = { id; filename; name = funcname; locs = [];
                              total_count = 0; n_allocs = 0; total_dist_to_alloc = 0 } in
          Hashtbl.add t.funcs (filename, funcname) func;
          func in
     let entry =
       { line; start_ch; end_ch; func; alloc_count = 0 } in
     Loc_tbl.add t.entries loc entry;
     func.locs <- entry :: func.locs;
     entry

module HH = Heavy_hitters(struct
  type t = func * func
  let hash ((a, b) : t) =
    a.id * 1231441 + b.id * 3821
  let equal ((a,b) : t) ((a',b') : t) = (a.id = a'.id) && (b.id = b'.id)
(*
  let hash ((a : location_code), (b : location_code)) =
    Int64.(shift_right (add (mul (a :> int64) 0x94837298472a9321L) (mul (b :> int64) 0x4783213feac37L)) 11 |> to_int)
  let equal (a, b) (a', b') =
    Int64.equal (a : location_code :> int64) (a' : location_code :> int64) &&
      Int64.equal (b : location_code :> int64) (b' : location_code:> int64)*)
end)


let count filename =
  let trace = Reader.open_ ~filename in
  let hh = HH.make 10000 in
  let seen = Func_tbl.create 100 in
  let locs = new_loc_table trace in
  let total_samples = ref 0 in
  Reader.iter trace (fun _time ev ->
      match ev with
      | Alloc {obj_id=_; length=_; nsamples; source=_;
               backtrace_buffer; backtrace_length; common_prefix=_} ->
         let allocpt = add_loc locs backtrace_buffer (backtrace_length - 1) in
         allocpt.alloc_count <- allocpt.alloc_count + nsamples;
         Func_tbl.clear seen;
         for i' = 0 to backtrace_length - 2 do
           let i = backtrace_length - 2 - i' in
           let b = (add_loc locs backtrace_buffer i).func in
           if not (Func_tbl.mem seen b) then begin
               Func_tbl.add seen b ();
               b.total_count <- b.total_count + nsamples;
               b.n_allocs <- b.n_allocs + 1;
               b.total_dist_to_alloc <- b.total_dist_to_alloc + (backtrace_length - 1 - i);
               HH.add hh (b, allocpt.func)
             end
         done;
         total_samples := !total_samples + nsamples
      | Promote _ -> ()
      | Collect _ -> ());
  let tinfo = Reader.info trace in
  Reader.close trace;
  let total_samples = !total_samples in
  let wordsize = 8. in  (* FIXME: store this in the trace *)
  let print_bytes ppf = function
    | n when n < 100. ->
      Printf.fprintf ppf "%4.0f B" n
    | n when n < 100. *. 1024. ->
      Printf.fprintf ppf "%4.1f kB" (n /. 1024.)
    | n when n < 100. *. 1024. *. 1024. ->
      Printf.fprintf ppf "%4.1f MB" (n /. 1024. /. 1024.)
    | n when n < 100. *. 1024. *. 1024. *. 1024. ->
      Printf.fprintf ppf "%4.1f GB" (n /. 1024. /. 1024. /. 1024.)
    | n ->
      Printf.fprintf ppf "%4.1f TB" (n /. 1024. /. 1024. /. 1024. /. 1024.) in
  Printf.printf "Trace for %s [%Ld]:\n   %d samples of %a allocations\n\n"
    tinfo.executable_name tinfo.pid
    total_samples
    print_bytes (float_of_int total_samples /. tinfo.sample_rate *. wordsize);
  let hot_allocs = Func_tbl.create 100 in
  HH.iter hh (fun (fn,al) d1 _d2 ->
      let alloc_prop = float_of_int (direct_allocs al) /. float_of_int total_samples in
      if alloc_prop > 0.005 then begin
          let callers =
            match Func_tbl.find hot_allocs al with
            | c -> c
            | exception Not_found ->
               let c = ref [] in
               Func_tbl.add hot_allocs al c;
               c in
          let pair_freq = float_of_int d1 in
          let fn_freq = float_of_int (total_allocs fn) in
          if pair_freq /. fn_freq > max 0.10 (alloc_prop *. 1.2)
          && pair_freq /. float_of_int total_samples > 0.005 then begin
              callers := (fn, d1) :: !callers
            end
        end);
  let hot_allocs = Func_tbl.fold (fun al c acc -> (al, !c) :: acc) hot_allocs [] in
  let hot_allocs = List.sort (fun (al,_) (al',_) -> compare (direct_allocs al') (direct_allocs al)) hot_allocs in
  hot_allocs |> List.iter (fun (al, callers) ->
    let freq = float_of_int (direct_allocs al) /. float_of_int total_samples in
    let bytes = float_of_int (direct_allocs al) /. tinfo.sample_rate *. wordsize in
    Printf.printf "%a (%4.1f%%) at %s" print_bytes bytes (100. *. freq) al.name;
    Printf.printf " (%s:" al.filename;
    let printed = ref 0 in
    let locs = al.locs |> List.sort (fun l1 l2 -> compare l2.alloc_count l1.alloc_count) in
    locs |> List.iter (fun { line; start_ch; end_ch; alloc_count; func=_ } ->
      if alloc_count > 0 then begin
          begin match !printed with
          | n when n < 3 ->
             if n > 0 then Printf.printf ", ";
             Printf.printf "%d:%d-%d" line start_ch end_ch
          | 3 -> Printf.printf "..."
          | _ -> () end;
          incr printed
        end);
    Printf.printf ")\n";
    let first_caller = ref true in

    let callers = List.sort (fun (_c,f) (_c',f') ->
      (*compare (avg_dist_to_alloc c) (avg_dist_to_alloc c')*)
      (* compare (float_of_int f' /. float_of_int (total_allocs c')) (float_of_int f /. float_of_int (total_allocs c)) *)
      compare f' f
    ) callers in
    callers |> List.iter (fun (caller,freq) ->
      let rfreq = float_of_int freq /. float_of_int (total_allocs caller) in
      (* let pfreq = float_of_int freq /. float_of_int total_samples in *)
      let bytes = float_of_int freq /. tinfo.sample_rate *. wordsize in
      if float_of_int (total_allocs caller) /. float_of_int total_samples > 0.005 then begin
        if !first_caller then Printf.printf "      including:\n";
        first_caller := false;
        Printf.printf "    %a via %s (%.0f%% of this)\n" print_bytes bytes caller.name (100. *. rfreq)
      end);
    Printf.printf "\n")


(*
    let pair_freq = d1 in
    let tot_freq = summarize_fn fn in
    if 100 * pair_freq / tot_freq > 20 then begin
      let frac_of_program = float_of_int pair_freq /. float_of_int !total_samples in
      Printf.printf "%.1f%% % 6d % 6d % 6d %d %s %s\n" (100. *. rf) !total_samples d1 d2 tot_freq fn.name al.name
    end)
 *)

let () =
  if Array.length Sys.argv <> 2 then
    Printf.fprintf stderr "Usage: %s <trace file>\n" Sys.executable_name
  else
    count Sys.argv.(1)
