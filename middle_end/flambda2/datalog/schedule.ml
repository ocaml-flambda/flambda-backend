(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** An ['a incremental] represents a value (database or table), paired with
    another copy representing the latest changes to the value. *)
type 'a incremental =
  { current : 'a;
    difference : 'a
  }

let incremental ~difference ~current = { current; difference }

let incremental_get f t = { current = f t.current; difference = f t.difference }

let incremental_set f v t =
  { current = f v.current t.current; difference = f v.difference t.difference }

(** A [binder] is a reference to the state of a single table during evaluation.
    It contains the state of the table at the start of evaluation, the current
    state of the table, and the difference between those. *)
type binder =
  | Binder :
      { table_id : ('t, 'k, 'v) Table.Id.t;
        previous : 't ref;
        current : 't incremental ref
      }
      -> binder

let print_binder ppf (Binder { table_id; _ }) = Table.Id.print ppf table_id

(** A rule consists of:

      - A [Cursor.t] to iterates on the entries produced by the right-hand
        side of the rule (hypotheses);

      - A list of [binder]s that are bound to the tables appearing in the
        left-hand side of the rule (conclusion) and updated throughout rule
        evaluation;

      - An unique identifier for logging/tracing purposes.

    The cursor embeds callbacks to update the [binder]s. *)
type rule =
  | Rule :
      { cursor : 'a Cursor.t;
        binders : binder list;
        rule_id : int
      }
      -> rule

type deduction =
  [ `Atom of Datalog.atom
  | `And of deduction list ]

let fresh_rule_id =
  let cnt = ref 0 in
  fun () ->
    incr cnt;
    !cnt

let find_or_create_ref (type t k v) binders (table_id : (t, k, v) Table.Id.t) :
    t incremental ref =
  let uid = Table.Id.uid table_id in
  match Hashtbl.find_opt binders uid with
  | None ->
    let empty = Trie.empty (Table.Id.is_trie table_id) in
    let current = ref (incremental ~difference:empty ~current:empty) in
    let previous = ref empty in
    Hashtbl.replace binders uid (Binder { table_id; previous; current });
    current
  | Some (Binder { table_id = other_table_id; previous = _; current }) ->
    let Equal = Table.Id.provably_equal_exn other_table_id table_id in
    current

let deduce (atoms : deduction) =
  let rec fold f atoms acc =
    match atoms with
    | `Atom atom -> f atom acc
    | `And atoms -> List.fold_left (fun acc atoms -> fold f atoms acc) acc atoms
  in
  let binders : (int, binder) Hashtbl.t = Hashtbl.create 17 in
  let callbacks =
    fold
      (fun (Datalog.Atom (tid, args)) callbacks ->
        let is_trie = Table.Id.is_trie tid in
        let table_ref = find_or_create_ref binders tid in
        Datalog.create_callback
          ~name:(Table.Id.name tid ^ ".insert")
          (fun keys ->
            let incremental_table = !table_ref in
            match Trie.find_opt is_trie keys incremental_table.current with
            | Some _ -> ()
            | None ->
              table_ref
                := incremental
                     ~current:
                       (Trie.add_or_replace is_trie keys ()
                          incremental_table.current)
                     ~difference:
                       (Trie.add_or_replace is_trie keys ()
                          incremental_table.difference))
          args
        :: callbacks)
      atoms []
  in
  let binders =
    Hashtbl.fold (fun _ binder binders -> binder :: binders) binders []
  in
  Datalog.map_program (Datalog.execute callbacks) (fun cursor ->
      let cursor = Cursor.With_parameters.without_parameters cursor in
      Rule { cursor; binders; rule_id = fresh_rule_id () })

type stats = (int, rule * float) Hashtbl.t

let create_stats () = Hashtbl.create 17

let add_timing ~stats (Rule { rule_id; _ } as rule) time =
  Hashtbl.replace stats rule_id
    (rule, time +. try snd (Hashtbl.find stats rule_id) with Not_found -> -0.)

let print_stats ppf stats =
  Format.fprintf ppf "@[<v>";
  Hashtbl.iter
    (fun _ (Rule { cursor; binders; _ }, time) ->
      Format.fprintf ppf "@[@[@[%a@]@ :- %a@]@,: %f@]@ "
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
           print_binder)
        binders Cursor.print cursor time)
    stats;
  Format.fprintf ppf "@]"

(** Evaluate a single rule using semi-naive evaluation.

    The [previous], [diff], and [current] parameters represent the state of the
    database in which we are evaluating the cursor (see the documentaion of
    {!Cursor.seminaive_run}).

    The [incremental_db] parameter is the database where we are accumulating the
    result of the rule, and its new value is returned.

    {b Note}: there needs not be any relationship between the input database
    represented by the [(previous, diff, current)] triple and the output
    database [incremental_db].
*)
let run_rule_incremental ?stats ~previous ~diff ~current incremental_db
    (Rule { binders; cursor; _ } as rule) =
  List.iter
    (fun (Binder { table_id; previous; current }) ->
      let incremental_table =
        incremental_get (Table.Map.get table_id) incremental_db
      in
      previous := incremental_table.current;
      current := incremental_table)
    binders;
  let time0 = Sys.time () in
  Cursor.seminaive_run cursor ~previous ~diff ~current;
  let time1 = Sys.time () in
  let seminaive_time = time1 -. time0 in
  Option.iter (fun stats -> add_timing ~stats rule seminaive_time) stats;
  List.fold_left
    (fun incremental_db (Binder { table_id; previous; current }) ->
      let previous = !previous and { current; difference } = !current in
      if previous == current
      then incremental_db
      else
        incremental_set (Table.Map.set table_id) { current; difference }
          incremental_db)
    incremental_db binders

type t =
  | Saturate of rule list
  | Fixpoint of t list

let fixpoint schedule = Fixpoint schedule

let saturate rules = Saturate rules

let run_rules_incremental ?stats rules ~previous ~diff ~current incremental_db =
  List.fold_left
    (run_rule_incremental ?stats ~previous ~diff ~current)
    incremental_db rules

(** Repeatedly apply the rules in [rules] to the database [current] until
    reaching a fixpoint.

    Returns an incremental database containing the new state of [current] along
    with all the new facts added during saturation.
*)
let saturate_rules_incremental ?stats rules ~previous ~diff ~current =
  let rec saturate_rules_incremental ?stats ~previous ~diff ~current rules
      full_diff =
    (* After one call to [run_rules_incremental], all deductions from facts in
       [current] have been processed, so we only need to keep evaluating rules
       with at least one fact in [incremental_db.difference]. *)
    let incremental_db =
      run_rules_incremental ?stats ~previous ~diff ~current rules
        (incremental ~current ~difference:Table.Map.empty)
    in
    if Table.Map.is_empty incremental_db.difference
    then incremental ~current ~difference:full_diff
    else
      saturate_rules_incremental ?stats ~previous:current
        ~diff:incremental_db.difference ~current:incremental_db.current rules
        (Table.Map.concat ~earlier:full_diff ~later:incremental_db.difference)
  in
  saturate_rules_incremental ?stats rules Table.Map.empty ~previous ~diff
    ~current

(** Run the evaluation functions in [fns] until reaching a fixpoint.
*)
let run_list_incremental fns ~previous ~diff ~current =
  (* Each evaluation of a rule that produced changes is associated with a
     timestamp (the initial used-provided [diff] is at timestamp [0]), and each
     evaluation function is associated with the state of the database last time
     it was run (initially [previous]) and the corresponding timestamp
     (initially [-1]).

     Before evaluating a function [fn], we compute the diff since its previous
     run by concatenating all the diffs with a higher timestamp. *)
  let rec cut ~cut_after result = function
    | [] -> result
    | (ts, diff) :: diffs ->
      if ts > cut_after
      then cut ~cut_after (Table.Map.concat ~earlier:diff ~later:result) diffs
      else result
  in
  let rec loop (current, diffs, ts, full_diff) fns =
    let (current, diffs, ts', full_diff), fns =
      List.fold_left_map
        (fun (db, diffs, ts, full_diff) (fn, previous, cut_after) ->
          let diff = cut ~cut_after Table.Map.empty diffs in
          let incremental_db = fn ~previous ~diff ~current:db in
          if Table.Map.is_empty incremental_db.difference
          then (db, diffs, ts, full_diff), (fn, db, ts)
          else
            let ts = ts + 1 in
            ( ( incremental_db.current,
                (ts, incremental_db.difference) :: diffs,
                ts,
                Table.Map.concat ~earlier:full_diff
                  ~later:incremental_db.difference ),
              (fn, incremental_db.current, ts) ))
        (current, diffs, ts, full_diff)
        fns
    in
    if ts' = ts
    then incremental ~current ~difference:full_diff
    else loop (current, diffs, ts', full_diff) fns
  in
  loop
    (current, [0, diff], 0, Table.Map.empty)
    (List.map (fun fn -> fn, previous, -1) fns)

let rec run_incremental ?stats schedule ~previous ~diff ~current =
  match schedule with
  | Saturate rules ->
    saturate_rules_incremental ?stats rules ~previous ~diff ~current
  | Fixpoint schedules ->
    run_list_incremental
      (List.map (run_incremental ?stats) schedules)
      ~previous ~diff ~current

let run ?stats schedule db =
  (run_incremental ?stats schedule ~previous:Table.Map.empty ~diff:db
     ~current:db)
    .current
