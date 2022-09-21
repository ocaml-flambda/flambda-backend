[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Cfg_regalloc_utils

let live_before : type a. a Cfg.instruction -> liveness -> Reg.Set.t =
 fun instr liveness ->
  match Cfg_dataflow.Instr.Tbl.find_opt liveness instr.id with
  | None -> fatal "no liveness information for instruction %d" instr.id
  | Some { Cfg_liveness.before; across = _ } -> before

let remove_deadcode (body : Instruction.t list) liveness used_after :
    Instruction.t list * bool =
  let body, _, changed =
    List.fold_right body ~init:([], used_after, false)
      ~f:(fun (instr : Instruction.t) (acc, used_after, changed) ->
        let before = live_before instr liveness in
        let is_deadcode =
          match instr.desc with
          | Op _ as op ->
            Cfg.is_pure_basic op
            && Reg.disjoint_set_array used_after instr.res
            && (not (Proc.regs_are_volatile instr.arg))
            && not (Proc.regs_are_volatile instr.res)
          | Reloadretaddr | Pushtrap _ | Poptrap | Prologue -> false
        in
        let acc = if is_deadcode then acc else instr :: acc in
        acc, before, changed || is_deadcode)
  in
  body, changed

let run cfg_with_liveness =
  let liveness = Cfg_with_liveness.liveness cfg_with_liveness in
  let invalidate =
    Cfg.fold_blocks (Cfg_with_liveness.cfg cfg_with_liveness) ~init:false
      ~f:(fun _label block changed ->
        let new_body, body_changed =
          remove_deadcode block.body liveness
            (live_before block.terminator liveness)
        in
        block.body <- new_body;
        changed || body_changed)
  in
  if invalidate then Cfg_with_liveness.invalidate_liveness cfg_with_liveness;
  cfg_with_liveness
