[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Cfg_regalloc_utils
module DLL = Flambda_backend_utils.Doubly_linked_list

let live_before : type a. a Cfg.instruction -> liveness -> Reg.Set.t =
 fun instr liveness ->
  match Cfg_dataflow.Instr.Tbl.find_opt liveness instr.id with
  | None -> fatal "no liveness information for instruction %d" instr.id
  | Some { Cfg_liveness.before; across = _ } -> before

let remove_deadcode (body : Cfg.basic_instruction_list) changed liveness
    used_after : unit =
  let used_after = ref used_after in
  DLL.filter_right body ~f:(fun (instr : Instruction.t) ->
      let before = live_before instr liveness in
      let is_deadcode =
        match instr.desc with
        | Op _ as op ->
          Cfg.is_pure_basic op
          && Reg.disjoint_set_array !used_after instr.res
          && (not (Proc.regs_are_volatile instr.arg))
          && not (Proc.regs_are_volatile instr.res)
        | Reloadretaddr | Pushtrap _ | Poptrap | Prologue -> false
      in
      used_after := before;
      changed := !changed || is_deadcode;
      not is_deadcode)

let run cfg_with_liveness =
  let liveness = Cfg_with_liveness.liveness cfg_with_liveness in
  let changed = ref false in
  Cfg.iter_blocks (Cfg_with_liveness.cfg cfg_with_liveness)
    ~f:(fun _label block ->
      remove_deadcode block.body changed liveness
        (live_before block.terminator liveness));
  if !changed then Cfg_with_liveness.invalidate_liveness cfg_with_liveness;
  cfg_with_liveness
