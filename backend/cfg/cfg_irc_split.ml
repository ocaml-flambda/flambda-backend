[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Cfg_regalloc_utils
open! Cfg_irc_utils
module State = Cfg_irc_state

let naive_split_points : Cfg_with_layout.t -> Instruction.id list =
 fun cfg_with_layout ->
  if irc_debug then log ~indent:1 "naive_split_points";
  fold_instructions cfg_with_layout ~init:[]
    ~instruction:(fun acc (instr : Instruction.t) ->
      (* CR xclerc for xclerc: we may want to split [heuristically] in more
         situations. *)
      let split =
        match instr.desc with
        | Call (P (External _) | F (Direct _)) -> true
        | _ -> false
      in
      if split
      then (
        if irc_debug then log ~indent:2 "should split at %d" instr.id;
        instr.id :: acc)
      else acc)
    ~terminator:(fun acc _term -> acc)
  |> List.rev

type naive_split_instr =
  { before : Instruction.t;
    after : Instruction.t;
    new_regs : Reg.t list
  }

let[@inline] naive_split_instr :
    State.t ->
    Reg.t Reg.Tbl.t ->
    Instruction.t ->
    Reg.t ->
    Reg.t list ->
    naive_split_instr =
 fun state subst instr reg new_regs ->
  let new_reg, new_regs =
    match Reg.Tbl.find_opt subst reg with
    | Some r -> r, new_regs
    | None ->
      let new_reg =
        make_temporary ~same_class_and_base_name_as:reg ~name_prefix:"split"
      in
      State.add_introduced_temporaries_one state new_reg;
      State.add_initial_one state new_reg;
      State.add_spilled_nodes state new_reg;
      Reg.Tbl.replace subst reg new_reg;
      new_reg, new_reg :: new_regs
  in
  let before =
    Move.make_instr Move.Plain
      ~id:(State.get_and_incr_instruction_id state)
      ~copy:instr ~from:reg ~to_:new_reg
  in
  let after =
    Move.make_instr Move.Plain
      ~id:(State.get_and_incr_instruction_id state)
      ~copy:instr ~from:new_reg ~to_:reg
  in
  { before; after; new_regs }

let[@inline] apply_regs : Reg.t Reg.Tbl.t -> Reg.t array -> Reg.t array =
 fun subst arr ->
  (* CR xclerc for xclerc: allocate iff necessary. *)
  Array.map arr ~f:(fun reg ->
      match Reg.Tbl.find_opt subst reg with None -> reg | Some reg' -> reg')

let[@inline] apply_instr : Reg.t Reg.Tbl.t -> Instruction.t -> unit =
 fun subst instr ->
  instr.arg <- apply_regs subst instr.arg;
  instr.res <- apply_regs subst instr.res

type naive_split_body =
  { body : Instruction.t list;
    split_points : Instruction.id list;
    new_regs : Reg.t list
  }

let rec naive_split_body :
    State.t ->
    liveness ->
    Reg.t Reg.Tbl.t ->
    Instruction.t list ->
    Instruction.t list ->
    Instruction.id list ->
    Reg.t list ->
    naive_split_body =
 fun state liveness subst acc body split_points new_regs ->
  match body, split_points with
  | [], _ -> { body = List.rev acc; split_points; new_regs }
  | _, [] -> { body = List.rev acc @ body; split_points = []; new_regs }
  | hd_body :: tl_body, hd_split_points :: tl_split_points ->
    if hd_body.Cfg.id = hd_split_points
    then (
      let live = Cfg_dataflow.Instr.Tbl.find liveness hd_body.Cfg.id in
      if irc_debug
      then (
        log ~indent:2 "splitting at %d" hd_split_points;
        Reg.Set.iter
          (fun reg -> log ~indent:3 "register %a is live" Printmach.reg reg)
          live.across);
      let acc, new_regs, instrs_after =
        Reg.Set.fold
          (fun reg (acc, new_regs, instrs_after) ->
            let { before; after; new_regs } =
              naive_split_instr state subst hd_body reg new_regs
            in
            before :: acc, new_regs, after :: instrs_after)
          live.across (acc, new_regs, [])
      in
      apply_instr subst hd_body;
      naive_split_body state liveness subst
        (instrs_after @ (hd_body :: acc))
        tl_body tl_split_points new_regs)
    else
      naive_split_body state liveness subst (hd_body :: acc) tl_body
        split_points new_regs

let naive_split_cfg :
    State.t ->
    Cfg_with_layout.t ->
    Instruction.id list ->
    liveness ->
    Reg.t list =
 fun state cfg_with_layout split_points liveness ->
  if irc_debug then log ~indent:1 "naive_split";
  let subst = Reg.Tbl.create 32 in
  let split_points, new_regs =
    Cfg.fold_blocks (Cfg_with_layout.cfg cfg_with_layout)
      ~init:(split_points, [])
      ~f:(fun label block ((split_points, new_regs) as acc) ->
        match split_points with
        | [] -> acc
        | hd :: _ ->
          if irc_debug then log ~indent:2 "splitting in #%d" label;
          if List.exists block.body ~f:(fun instr -> instr.Cfg.id = hd)
          then (
            let { body; split_points; new_regs } =
              naive_split_body state liveness subst [] block.body split_points
                new_regs
            in
            block.body <- body;
            split_points, new_regs)
          else acc)
  in
  assert (split_points = []);
  new_regs
