[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Cfg_regalloc_utils
open! Cfg_irc_utils
module State = Cfg_irc_state

let is_naive_split_point : Instruction.t -> bool =
 fun instr ->
  (* CR xclerc for xclerc: we may want to split [heuristically] in more
     situations. *)
  match instr.desc with
  | Call (P (External _) | F (Direct _)) -> true
  | _ -> false

let[@inline] apply_regs : Reg.t Reg.Tbl.t -> Reg.t array -> Reg.t array =
 fun subst arr ->
  (* CR xclerc for xclerc: allocate iff necessary. *)
  Array.map arr ~f:(fun reg ->
      match Reg.Tbl.find_opt subst reg with None -> reg | Some reg' -> reg')

let[@inline] apply_instr : Reg.t Reg.Tbl.t -> Instruction.t -> unit =
 fun subst instr ->
  instr.arg <- apply_regs subst instr.arg;
  instr.res <- apply_regs subst instr.res

let[@inline] naive_split_instr :
    State.t ->
    liveness ->
    Reg.t list ref ->
    Reg.t Reg.Tbl.t ->
    Cfg.BasicInstructionList.cell ->
    unit =
 fun state liveness new_regs subst cell ->
  let instr = Cfg.BasicInstructionList.instr cell in
  if is_naive_split_point instr
  then (
    let live = Cfg_dataflow.Instr.Tbl.find liveness instr.id in
    if irc_debug
    then (
      log ~indent:2 "splitting at %d" instr.id;
      Reg.Set.iter
        (fun reg -> log ~indent:3 "register %a is live" Printmach.reg reg)
        live.across);
    Reg.Set.iter
      (fun reg ->
        let new_reg =
          match Reg.Tbl.find_opt subst reg with
          | Some r -> r
          | None ->
            let new_reg =
              make_temporary ~same_class_and_base_name_as:reg
                ~name_prefix:"split"
            in
            State.add_introduced_temporaries_one state new_reg;
            State.add_initial_one state new_reg;
            State.add_spilled_nodes state new_reg;
            Reg.Tbl.replace subst reg new_reg;
            new_regs := new_reg :: !new_regs;
            new_reg
        in
        Cfg.BasicInstructionList.insert_before cell
          (Move.make_instr Move.Plain
             ~id:(State.get_and_incr_instruction_id state)
             ~copy:instr ~from:reg ~to_:new_reg);
        Cfg.BasicInstructionList.insert_after cell
          (Move.make_instr Move.Plain
             ~id:(State.get_and_incr_instruction_id state)
             ~copy:instr ~from:new_reg ~to_:reg);
        apply_instr subst instr)
      live.across)

let naive_split_cfg : State.t -> Cfg_with_layout.t -> liveness -> Reg.t list =
 fun state cfg_with_layout liveness ->
  if irc_debug then log ~indent:1 "naive_split";
  let new_regs = ref [] in
  let subst = Reg.Tbl.create 32 in
  Cfg.iter_blocks (Cfg_with_layout.cfg cfg_with_layout) ~f:(fun label block ->
      if irc_debug then log ~indent:2 "splitting in #%d" label;
      Cfg.BasicInstructionList.iter_cell block.body ~f:(fun cell ->
          naive_split_instr state liveness new_regs subst cell));
  !new_regs
