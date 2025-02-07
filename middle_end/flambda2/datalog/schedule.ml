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

type 'a incremental =
  { current : 'a;
    difference : 'a
  }

type binder =
  | Binder :
      { table_id : ('t, 'k, 'v) Table.Id.t;
        initial : 't incremental ref;
        current : 't incremental ref
      }
      -> binder

let print_binder ppf (Binder { table_id; _ }) = Table.Id.print ppf table_id

type rule =
  | Rule :
      { cursor : 'a Cursor.t;
        binders : binder list;
        rule_id : int
      }
      -> rule

(* Rule identifiers are only used for statistics collection at the moment. *)
let fresh_rule_id =
  let cnt = ref 0 in
  fun () ->
    incr cnt;
    !cnt

type stats = (int, rule * float) Hashtbl.t

let create_stats () = Hashtbl.create 17

let add_timing ~stats (Rule { rule_id; _ } as rule) time =
  Hashtbl.replace stats rule_id
    (rule, time +. try snd (Hashtbl.find stats rule_id) with Not_found -> -0.)

let print_stats ppf stats =
  Format.fprintf ppf "@[<v>";
  Hashtbl.iter
    (fun _ (Rule { cursor; binders; _ }, time) ->
      Format.fprintf ppf "@[@[@[%a@]@ :- %a@]:@ %f@]@ "
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
           print_binder)
        binders Cursor.print cursor time)
    stats;
  Format.fprintf ppf "@]"

let incremental ~difference ~current = { current; difference }

type builder = { tables : (int, binder) Hashtbl.t }

let create_builder () = { tables = Hashtbl.create 17 }

type 'k rule_fn = 'k Heterogenous_list.Constant.hlist -> unit

let call_rule_fn f args = f args

let find_or_create_ref (type t k v) { tables } (table_id : (t, k, v) Table.Id.t)
    : t incremental ref =
  let uid = Table.Id.uid table_id in
  match Hashtbl.find_opt tables uid with
  | None ->
    let empty = Trie.empty (Table.Id.is_trie table_id) in
    let initial = ref (incremental ~difference:empty ~current:empty) in
    let current = ref !initial in
    Hashtbl.replace tables uid (Binder { table_id; initial; current });
    current
  | Some (Binder { table_id = other_table_id; initial = _; current }) ->
    let Equal = Table.Id.provably_equal_exn other_table_id table_id in
    current

let add_rule builder tid : _ rule_fn =
  let is_trie = Table.Id.is_trie tid in
  let table_ref = find_or_create_ref builder tid in
  fun keys ->
    let incremental_table = !table_ref in
    match Trie.find_opt is_trie keys incremental_table.current with
    | Some _ -> ()
    | None ->
      table_ref
        := incremental
             ~current:
               (Trie.add_or_replace is_trie keys () incremental_table.current)
             ~difference:
               (Trie.add_or_replace is_trie keys () incremental_table.difference)

let build builder cursor =
  let binders =
    Hashtbl.fold (fun _ binder binders -> binder :: binders) builder.tables []
  in
  Rule { cursor; binders; rule_id = fresh_rule_id () }

let run_rule_incremental ?stats ~previous ~diff ~current incremental_db
    (Rule { binders; cursor; _ } as rule) =
  List.iter
    (fun (Binder { table_id; initial; current }) ->
      let table =
        incremental
          ~current:(Table.Map.get table_id incremental_db.current)
          ~difference:(Table.Map.get table_id incremental_db.difference)
      in
      initial := table;
      current := table)
    binders;
  let time0 = Sys.time () in
  Cursor.seminaive_iter cursor ~previous ~diff ~current (fun _ -> ());
  let time1 = Sys.time () in
  let seminaive_time = time1 -. time0 in
  Option.iter (fun stats -> add_timing ~stats rule seminaive_time) stats;
  let set_if_changed db table_id ~before ~after =
    if after == before then db else Table.Map.set table_id after db
  in
  List.fold_left
    (fun incremental_db (Binder { table_id; initial; current }) ->
      let initial = !initial and current = !current in
      incremental
        ~current:
          (set_if_changed incremental_db.current table_id
             ~before:initial.current ~after:current.current)
        ~difference:
          (set_if_changed incremental_db.difference table_id
             ~before:initial.difference ~after:current.difference))
    incremental_db binders

type t =
  | Saturate of rule list
  | Fixpoint of t list

let fixpoint schedule = Fixpoint schedule

let saturate rules = Saturate rules

let run_rules_incremental ?stats rules ~previous ~diff ~current =
  List.fold_left
    (run_rule_incremental ?stats ~previous ~diff ~current)
    (incremental ~current ~difference:Table.Map.empty)
    rules

let rec saturate_rules_incremental ?stats ~previous ~diff ~current rules
    full_diff =
  let incremental_db =
    run_rules_incremental ?stats ~previous ~diff ~current rules
  in
  if Table.Map.is_empty incremental_db.difference
  then incremental ~current ~difference:full_diff
  else
    saturate_rules_incremental ?stats ~previous:current
      ~diff:incremental_db.difference ~current:incremental_db.current rules
      (Table.Map.concat ~earlier:full_diff ~later:incremental_db.difference)

let saturate_rules_incremental ?stats rules ~previous ~diff ~current =
  saturate_rules_incremental ?stats rules Table.Map.empty ~previous ~diff
    ~current

let run_list_incremental fns ~previous ~diff ~current =
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
