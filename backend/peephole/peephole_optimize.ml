[@@@ocaml.warning "+a-29-40-41-42"]

open! Peephole_utils
open! Peephole_rules

(* Helper function for optimize_body. Here cell is an iterator of the doubly
   linked list data structure that encapsulates the body's instructions. *)
let rec optimize_body' cell made_optimizations =
  match handbuilt_rules cell with
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
  let made_optimizations =
    Label.Tbl.fold
      (fun (_ : Label.t) (block : Cfg.basic_block) (made_optimizations : bool) ->
        made_optimizations || optimize_body block.body)
      (Cfg_with_layout.cfg cfg_with_layout).blocks false
  in
  cfg_with_layout, made_optimizations
