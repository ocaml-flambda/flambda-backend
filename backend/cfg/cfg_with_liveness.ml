[@@@ocaml.warning "+a-30-40-41-42"]

open! Cfg_regalloc_utils

type t =
  { cfg_with_layout : Cfg_with_layout.t;
    mutable liveness : liveness option
  }

let make cfg_with_layout = { cfg_with_layout; liveness = None }

let cfg_with_layout t = t.cfg_with_layout

let cfg t = Cfg_with_layout.cfg t.cfg_with_layout

let[@inline] compute_liveness_if_necessary t =
  match t.liveness with
  | Some liveness -> liveness
  | None ->
    let liveness =
      Profile.record ~accumulate:true "liveness_analysis" liveness_analysis
        t.cfg_with_layout
    in
    t.liveness <- Some liveness;
    liveness

let liveness t = compute_liveness_if_necessary t

let liveness_find t id =
  Cfg_dataflow.Instr.Tbl.find (compute_liveness_if_necessary t) id

let liveness_find_opt t id =
  Cfg_dataflow.Instr.Tbl.find_opt (compute_liveness_if_necessary t) id

let invalidate_liveness t = t.liveness <- None
