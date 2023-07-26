[@@@ocaml.warning "+a-29-40-41-42"]

open! Peephole_utils

(* Helper function for optimize_body. Here cell is an iterator of the doubly
   linked list data structure that encapsulates the body's instructions. *)
let rec optimize_body' cell made_optimizations =
  match List.find_map (fun opt_func -> opt_func cell) Peephole_rules.optimizations with
  | None -> (
    match DLL.next cell with
    | None -> made_optimizations
    | Some next_cell -> optimize_body' next_cell made_optimizations)
  | Some continuation_cell -> optimize_body' continuation_cell true

let optimize_body (body : Cfg.basic_instruction_list) =
  match DLL.hd_cell body with
  | Some cell -> optimize_body' cell false
  | None -> false

(* Apply peephole optimization for the body of each block of the CFG*)
let peephole_optimize_cfg cfg_with_layout =
  let fun_name = (Cfg_with_layout.cfg cfg_with_layout).fun_name in
  if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
  then (
    set_csv ();
    IntCsv.add_empty_row (get_csv ()) fun_name);
  let made_optimizations =
    Label.Tbl.fold
      (fun (_ : Label.t) (block : Cfg.basic_block) (made_optimizations : bool) ->
        made_optimizations || optimize_body block.body)
      (Cfg_with_layout.cfg cfg_with_layout).blocks false
  in
  cfg_with_layout, made_optimizations
