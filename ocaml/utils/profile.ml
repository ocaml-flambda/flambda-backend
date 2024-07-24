(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Pierre Chambart, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-18-40-42-48"]

type file = string

module Int = Misc.Stdlib.Int
module String = Misc.Stdlib.String

module Counters = struct
  type t = int String.Map.t

  let create () = String.Map.empty
  let get name t = String.Map.find_opt name t |> Option.value ~default:0
  let set name count t = String.Map.add name count t
  let is_empty = String.Map.is_empty
  let union = String.Map.union (fun _ count1 count2 -> Some (count1 + count2))
  let to_string t =
    t
    |> String.Map.bindings
    |> List.map (fun (name, count) -> Printf.sprintf "%s = %d" name count)
    |> String.concat "; "
    |> Printf.sprintf "[%s]"
end

external time_include_children: bool -> float = "caml_sys_time_include_children"
let cpu_time () = time_include_children true


module Measure = struct
  type t = {
    time : float;
    allocated_words : float;
    top_heap_words : int;
    counters : Counters.t;
  }
  let create ?(counters = Counters.create ()) () =
    let stat = Gc.quick_stat () in
    {
      time = cpu_time ();
      allocated_words = stat.minor_words +. stat.major_words;
      top_heap_words = stat.top_heap_words;
      counters = counters;
    }
  let zero = { time = 0.; allocated_words = 0.; top_heap_words = 0; counters = Counters.create () }
end

module Measure_diff = struct
  let timestamp = let r = ref (-1) in fun () -> incr r; !r
  type t = {
    timestamp : int;
    duration : float;
    allocated_words : float;
    top_heap_words_increase : int;
    counters : Counters.t;
  }
  let zero () = {
    timestamp = timestamp ();
    duration = 0.;
    allocated_words = 0.;
    top_heap_words_increase = 0;
    counters = Counters.create ();
  }
  let accumulate t (m1 : Measure.t) (m2 : Measure.t) = {
    timestamp = t.timestamp;
    duration = t.duration +. (m2.time -. m1.time);
    allocated_words =
      t.allocated_words +. (m2.allocated_words -. m1.allocated_words);
    top_heap_words_increase =
      t.top_heap_words_increase + (m2.top_heap_words - m1.top_heap_words);
    counters = Counters.union t.counters m2.counters
  }
  let of_diff m1 m2 =
    accumulate (zero ()) m1 m2
end

type hierarchy =
  | E of (string, Measure_diff.t * hierarchy) Hashtbl.t
[@@unboxed]

let create () = E (Hashtbl.create 2)
let hierarchy = ref (create ())
let initial_measure = ref None
let reset () = hierarchy := create (); initial_measure := None

let record_call_internal ?(accumulate = false) ?counter_f name f =
  let E prev_hierarchy = !hierarchy in
  let start_measure = Measure.create () in
  if !initial_measure = None then initial_measure := Some start_measure;
  let this_measure_diff, this_table =
    (* We allow the recording of multiple categories by the same name, for tools
       like ocamldoc that use the compiler libs but don't care about profile
       information, and so may record, say, "parsing" multiple times. *)
    if accumulate
    then
      match Hashtbl.find prev_hierarchy name with
      | exception Not_found -> Measure_diff.zero (), Hashtbl.create 2
      | measure_diff, E table ->
        Hashtbl.remove prev_hierarchy name;
        measure_diff, table
    else Measure_diff.zero (), Hashtbl.create 2
  in
  hierarchy := E this_table;
  let counters = ref (Counters.create ()) in
  Misc.try_finally (
    match counter_f with
    | Some counter_f ->
        fun () ->
          let result = f () in
          if List.mem `Counters !Clflags.profile_columns then
            counters := counter_f result;
          result
    | None -> f
    )
    ~always:(fun () ->
        hierarchy := E prev_hierarchy;
        let end_measure = Measure.create ~counters:(!counters) () in
        let measure_diff =
          Measure_diff.accumulate this_measure_diff start_measure end_measure in
        Hashtbl.add prev_hierarchy name (measure_diff, E this_table))

let record_call = record_call_internal ?counter_f:None

let record ?accumulate pass f x = record_call ?accumulate pass (fun () -> f x)

let record_with_counters ?accumulate ~counter_f pass f x =
  record_call_internal ?accumulate ~counter_f pass (fun () -> f x)

type display = {
  to_string : max:float -> width:int -> string;
  worth_displaying : max:float -> bool;
}

let time_display precision v : display =
  (* Because indentation is meaningful, and because the durations are
     the first element of each row, we can't pad them with spaces. *)
  let to_string_without_unit v ~width = Printf.sprintf "%0*.*f" width precision v in
  let to_string ~max:_ ~width =
    to_string_without_unit v ~width:(width - 1) ^ "s" in
  let worth_displaying ~max:_ =
    float_of_string (to_string_without_unit v ~width:0) <> 0. in
  { to_string; worth_displaying }

let memory_word_display =
  (* To make memory numbers easily comparable across rows, we choose a single
     scale for an entire column. To keep the display compact and not overly
     precise (no one cares about the exact number of bytes), we pick the largest
     scale we can and we only show 3 digits. Avoiding showing tiny numbers also
     allows us to avoid displaying passes that barely allocate compared to the
     rest of the compiler.  *)
  let bytes_of_words words = words *. float_of_int (Sys.word_size / 8) in
  let to_string_without_unit v ~width scale =
    let precision = 3 and precision_power = 1e3 in
    let v_rescaled = bytes_of_words v /. scale in
    let v_rounded =
      floor (v_rescaled *. precision_power +. 0.5) /. precision_power in
    let v_str = Printf.sprintf "%.*f" precision v_rounded in
    let index_of_dot = String.index v_str '.' in
    let v_str_truncated =
      String.sub v_str 0
        (if index_of_dot >= precision
         then index_of_dot
         else precision + 1)
    in
    Printf.sprintf "%*s" width v_str_truncated
  in
  let choose_memory_scale =
    let units = [|"B"; "kB"; "MB"; "GB"|] in
    fun words ->
      let bytes = bytes_of_words words in
      let scale = ref (Array.length units - 1) in
      while !scale > 0 && bytes < 1024. ** float_of_int !scale do
        decr scale
      done;
      1024. ** float_of_int !scale, units.(!scale)
  in
  fun ?previous v : display ->
    let to_string ~max ~width =
      let scale, scale_str = choose_memory_scale max in
      let width = width - String.length scale_str in
      to_string_without_unit v ~width scale ^ scale_str
    in
    let worth_displaying ~max =
      let scale, _ = choose_memory_scale max in
      float_of_string (to_string_without_unit v ~width:0 scale) <> 0.
      && match previous with
      | None -> true
      | Some p ->
         (* This branch is for numbers that represent absolute quantity, rather
            than differences. It allows us to skip displaying the same absolute
            quantity many times in a row. *)
         to_string_without_unit p ~width:0 scale
         <> to_string_without_unit v ~width:0 scale
    in
    { to_string; worth_displaying }

let counters_display counters  =
  let to_string ~max:_ ~width:_ = Counters.to_string counters in
  let worth_displaying ~max:_ = not (Counters.is_empty counters) in
  0., { to_string; worth_displaying }

let profile_list (E table) =
  let l = Hashtbl.fold (fun k d l -> (k, d) :: l) table [] in
  List.sort (fun (_, (p1, _)) (_, (p2, _)) ->
    compare p1.Measure_diff.timestamp p2.Measure_diff.timestamp) l

let compute_other_category (E table : hierarchy) (total : Measure_diff.t) =
  let r = ref total in
  Hashtbl.iter (fun _pass ((p2 : Measure_diff.t), _) ->
    let p1 = !r in
    r := {
      timestamp = p1.timestamp;
      duration = p1.duration -. p2.duration;
      allocated_words = p1.allocated_words -. p2.allocated_words;
      top_heap_words_increase =
        p1.top_heap_words_increase - p2.top_heap_words_increase;
      counters = Counters.create ();
    }
  ) table;
  !r

type row = R of string * (float * display) list * row list

let rec rows_of_hierarchy ~nesting make_row name measure_diff hierarchy env =
  let rows =
    rows_of_hierarchy_list
      ~nesting:(nesting + 1) make_row hierarchy measure_diff env in
  let values, env =
    make_row env measure_diff ~toplevel_other:(nesting = 0 && name = "other") in
  R (name, values, rows), env

and rows_of_hierarchy_list ~nesting make_row hierarchy total env =
  let list = profile_list hierarchy in
  let list =
    if list <> [] || nesting = 0
    then list @ [ "other", (compute_other_category hierarchy total, create ()) ]
    else []
  in
  let env = ref env in
  List.map (fun (name, (measure_diff, hierarchy)) ->
    let a, env' =
      rows_of_hierarchy ~nesting make_row name measure_diff hierarchy !env in
    env := env';
    a
  ) list

let rows_of_hierarchy hierarchy measure_diff initial_measure columns timings_precision =
  (* Computing top heap size is a bit complicated: if the compiler applies a
     list of passes n times (rather than applying pass1 n times, then pass2 n
     times etc), we only show one row for that pass but what does "top heap
     size at the end of that pass" even mean?
     It seems the only sensible answer is to pretend the compiler applied pass1
     n times, pass2 n times by accumulating all the heap size increases that
     happened during each pass, and then compute what the heap size would have
     been. So that's what we do.
     There's a bit of extra complication, which is that the heap can increase in
     between measurements. So the heap sizes can be a bit off until the "other"
     rows account for what's missing. We special case the toplevel "other" row
     so that any increases that happened before the start of the compilation is
     correctly reported, as a lot of code may run before the start of the
     compilation (eg functor applications). *)
    let make_row prev_top_heap_words (p : Measure_diff.t) ~toplevel_other =
      let top_heap_words =
        prev_top_heap_words
        + p.top_heap_words_increase
        - if toplevel_other
          then initial_measure.Measure.top_heap_words
          else 0
      in
      let make value ~f = value, f value in
      List.map (function
        | `Time ->
          make p.duration ~f:(time_display timings_precision)
        | `Alloc ->
          make p.allocated_words ~f:memory_word_display
        | `Top_heap ->
          make (float_of_int p.top_heap_words_increase) ~f:memory_word_display
        | `Abs_top_heap ->
          make (float_of_int top_heap_words)
           ~f:(memory_word_display ~previous:(float_of_int prev_top_heap_words))
        | `Counters -> counters_display p.counters
      ) columns,
      top_heap_words
  in
  rows_of_hierarchy_list ~nesting:0 make_row hierarchy measure_diff
    initial_measure.top_heap_words

let max_by_column ~n_columns rows =
  let a = Array.make n_columns 0. in
  let rec loop (R (_, values, rows)) =
    List.iteri (fun i (v, _) -> a.(i) <- Float.max a.(i) v) values;
    List.iter loop rows
  in
  List.iter loop rows;
  a

let width_by_column ~n_columns ~display_cell rows =
  let a = Array.make n_columns 1 in
  let rec loop (R (_, values, rows)) =
    List.iteri (fun i cell ->
      let _, str = display_cell i cell ~width:0 in
      a.(i) <- Int.max a.(i) (String.length str)
    ) values;
    List.iter loop rows;
  in
  List.iter loop rows;
  a

let output_rows
    ~(output_row : prefix:string -> cell_strings:string list -> name:string -> unit)
    ~(new_prefix : prev:string -> curr_name:string -> string)
    ~(always_output_ancestors : bool)
    ~(pad_empty : bool)
    rows
  =
  let n_columns =
    match rows with
    | [] -> 0
    | R (_, values, _) :: _ -> List.length values
  in
  let maxs = max_by_column ~n_columns rows in
  let display_cell i (_, c) ~width =
    let display_cell = c.worth_displaying ~max:maxs.(i) in
    display_cell, if display_cell
                  then c.to_string ~max:maxs.(i) ~width
                  else if pad_empty then String.make width '-' else ""
  in
  let widths = width_by_column ~n_columns ~display_cell rows in
  (* We track print row functions in a queue to ensure ancestors not worth displaying have
  print functions executed if a descendant is worth displaying (possible with counters) *)
  let rec loop (R (name, values, rows)) ~prefix ~output_stack =
    let worth_displaying, cell_strings =
      values
      |> List.mapi (fun i cell -> display_cell i cell ~width:widths.(i))
      |> List.split
    in
    let should_output_row = List.exists (fun b -> b) worth_displaying in
    let output_current () =
      let cell_strings = if should_output_row then cell_strings else [] in
      output_row ~prefix ~cell_strings ~name
    in
    output_stack := output_current :: (if always_output_ancestors then !output_stack else []);
    if should_output_row then
      (List.rev !output_stack |> List.iter (fun f -> f ()); output_stack := []);
    List.iter (loop ~prefix:(new_prefix ~prev:prefix ~curr_name:name) ~output_stack) rows;
    if !output_stack <> [] then output_stack := List.tl !output_stack
  in
  List.iter (loop ~prefix:"" ~output_stack:(ref [])) rows

let output_columns output_rows_f columns ~timings_precision =
  match columns with
  | [] -> ()
  | _ :: _ ->
     let initial_measure =
       match !initial_measure with
       | Some v -> v
       | None -> Measure.zero
     in
     let total = Measure_diff.of_diff Measure.zero (Measure.create ()) in
     output_rows_f (rows_of_hierarchy !hierarchy total initial_measure columns timings_precision)

let print ppf =
  output_rows
    ~output_row:(fun ~prefix ~cell_strings ~name ->
      Format.fprintf ppf "%s%s %s@\n" prefix (String.concat " " cell_strings) name)
    ~new_prefix:(fun ~prev ~curr_name:_ -> "  " ^ prev)
    ~always_output_ancestors:true
    ~pad_empty:true
  |> output_columns

let column_mapping = [
  `Time, "time";
  `Alloc, "alloc";
  `Top_heap, "top-heap";
  `Abs_top_heap, "absolute-top-heap";
  `Counters, "counters"
]

let output_to_csv ppf columns =
  let sanitise = String.map (fun c -> if c = ',' then '_' else c) in
  let to_csv cell_strings = cell_strings |> List.map sanitise |> String.concat "," in
  let string_columns = List.map (fun col -> List.assoc col column_mapping) columns in
  Format.fprintf ppf "%s@\n" (to_csv ("pass name" :: string_columns));
  let output_row_f = output_rows
    ~output_row:(fun ~prefix ~cell_strings ~name ->
      Format.fprintf ppf "%s%s@\n" prefix (to_csv (name :: cell_strings)))
    ~new_prefix:(fun ~prev ~curr_name -> Format.sprintf "%s%s/" prev curr_name)
    ~always_output_ancestors:false
    ~pad_empty:false
  in output_columns output_row_f columns

let all_columns = List.map fst column_mapping
let column_names = List.map snd column_mapping

let options_doc =
  Printf.sprintf
    " Print performance information for each pass\
   \n    The columns are: %s."
    (String.concat " " column_names)

let generate = "generate"
let transl = "transl"
let typing = "typing"
