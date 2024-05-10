(* Iterators for the minimizer *)

open Utils

type 'a minimized_step_result =
  | New_state of 'a (* New (smaller) states that produces an error *)
  | Change_removes_error
    (* This change removes the error, but other changes might be possible *)
  | No_more_changes
(* The last possible position for changes has been reached *)

let minimize_basic (state : 'a) (f : 'a -> pos:int -> 'a minimized_step_result)
    : 'a * bool =
  let rec aux (state : 'a) (pos : int) (ever_changed : bool) =
    match f state ~pos with
    | New_state nstate -> aux nstate pos true
    | Change_removes_error -> aux state (pos + 1) ever_changed
    | No_more_changes -> (state, ever_changed)
  in
  aux state 0 false

let minimize_ranged (state : 'a)
    (f : 'a -> pos:int -> len:int -> 'a minimized_step_result) : 'a * bool =
  let rec loop_increasing (state : 'a) (pos : int) (len : int)
      (ever_changed : bool) =
    match f state ~pos ~len with
    | New_state nstate -> loop_increasing nstate pos (2 * len) true
    | Change_removes_error -> loop_decreasing state pos (len / 2) ever_changed
    | No_more_changes -> (state, ever_changed)
  and loop_decreasing (state : 'a) (pos : int) (len : int) (ever_changed : bool)
      =
    if len = 0 then loop_increasing state (pos + 1) 1 ever_changed
    else
      match f state ~pos ~len with
      | New_state nstate -> loop_decreasing nstate pos (len / 2) true
      | Change_removes_error -> loop_decreasing state pos (len / 2) ever_changed
      | No_more_changes -> (state, ever_changed)
  in
  loop_increasing state 0 1 false

let minimize_at minimize cur_file map ~pos ~len =
  let r = ref (-1) in
  let nmap =
    minimize.minimizer_func
      (fun () ->
        incr r;
        pos <= !r && !r < pos + len)
      map cur_file
  in
  (nmap, pos <= !r)

let step_minimizer c minimize cur_file map ~pos ~len =
  Format.eprintf "Trying %s: pos=%d, len=%d... " minimize.minimizer_name pos len;
  let map, changed = minimize_at minimize cur_file map ~pos ~len in
  let r =
    if changed then (
      update_output map;
      if raise_error c then (
        save_outputs map;
        New_state map)
      else Change_removes_error)
    else No_more_changes
  in
  let () =
    match r with
    | New_state _ -> Format.eprintf "Reduced.@."
    | Change_removes_error -> Format.eprintf "Removes error.@."
    | No_more_changes -> Format.eprintf "No more changes.@."
  in
  r

(** [apply_minimizer test map cur_file minimize c] applies [minimize] for [cur_file]
     in the file set [mapi] for the command [c] as much as possible if (not !test),
     only once otherwise *)
let dicho = true

let apply_minimizer test map cur_file minimize (c : string) =
  let result, has_changed =
    if test then minimize_at minimize cur_file map ~pos:0 ~len:1
    else if dicho then minimize_ranged map (step_minimizer c minimize cur_file)
    else minimize_basic map (step_minimizer c minimize cur_file ~len:1)
  in
  update_output result;
  (result, has_changed)
