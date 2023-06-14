[@@@ocaml.warning "+a-30-40-41-42"]

type liveness = Cfg_liveness.Liveness.domain Cfg_dataflow.Instr.Tbl.t

let liveness_analysis : Cfg_with_layout.t -> liveness =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let init = { Cfg_liveness.before = Reg.Set.empty; across = Reg.Set.empty } in
  match
    Cfg_liveness.Liveness.run cfg ~init ~map:Cfg_liveness.Liveness.Instr ()
  with
  | Ok liveness -> liveness
  | Aborted _ -> .
  | Max_iterations_reached ->
    Misc.fatal_errorf "Unable to compute liveness from CFG for function %s@."
      cfg.Cfg.fun_name

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
    let liveness = liveness_analysis t.cfg_with_layout in
    t.liveness <- Some liveness;
    liveness

let liveness t = compute_liveness_if_necessary t

let liveness_find t id =
  Cfg_dataflow.Instr.Tbl.find (compute_liveness_if_necessary t) id

let liveness_find_opt t id =
  Cfg_dataflow.Instr.Tbl.find_opt (compute_liveness_if_necessary t) id

let invalidate_liveness t = t.liveness <- None
