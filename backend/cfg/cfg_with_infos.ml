[@@@ocaml.warning "+a-30-40-41-42"]

type liveness = Cfg_liveness.Liveness.domain Cfg_dataflow.Instr.Tbl.t

let liveness_analysis : Cfg_with_layout.t -> liveness =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let init = Cfg_liveness.Domain.bot in
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
    liveness : liveness option ref;
    dominators : Cfg_dominators.t option ref;
    loop_infos : Cfg_loop_infos.t option ref
  }

let make cfg_with_layout =
  { cfg_with_layout;
    liveness = ref None;
    dominators = ref None;
    loop_infos = ref None
  }

let cfg_with_layout t = t.cfg_with_layout

let cfg t = Cfg_with_layout.cfg t.cfg_with_layout

let fold_blocks t ~f ~init = Cfg.fold_blocks (cfg t) ~f ~init

let fold_body_instructions t = Cfg.fold_body_instructions (cfg t)

let get_block_exn t label = Cfg.get_block_exn (cfg t) label

let[@inline] compute_if_necessary r ~f =
  match !r with
  | Some value -> value
  | None ->
    let value = f () in
    r := Some value;
    value

let liveness t =
  compute_if_necessary t.liveness ~f:(fun () ->
      liveness_analysis t.cfg_with_layout)

let liveness_find t id = Cfg_dataflow.Instr.Tbl.find (liveness t) id

let liveness_find_opt t id = Cfg_dataflow.Instr.Tbl.find_opt (liveness t) id

let invalidate_liveness t = t.liveness := None

let dominators t =
  compute_if_necessary t.dominators ~f:(fun () -> Cfg_dominators.build (cfg t))

let loop_infos t =
  compute_if_necessary t.loop_infos ~f:(fun () ->
      Cfg_loop_infos.build (cfg t) (dominators t))

let invalidate_dominators_and_loop_infos t =
  t.dominators := None;
  t.loop_infos := None
