[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
open! Regalloc_utils
open! Regalloc_gi_utils

type t =
  { mutable assignments : Hardware_register.location Reg.Map.t;
    mutable introduced_temporaries : Reg.Set.t;
    stack_slots : Regalloc_stack_slots.t;
    instruction_id : InstructionId.sequence;
    initial_temporaries : int
  }

let[@inline] make ~initial_temporaries ~stack_slots ~last_used =
  let assignments = Reg.Map.empty in
  let instruction_id = InstructionId.make_sequence ~last_used () in
  let introduced_temporaries = Reg.Set.empty in
  { assignments;
    introduced_temporaries;
    stack_slots;
    instruction_id;
    initial_temporaries
  }

let[@inline] add_assignment state reg ~to_ =
  state.assignments <- Reg.Map.add reg to_ state.assignments

let[@inline] remove_assignment state reg =
  state.assignments <- Reg.Map.remove reg state.assignments

let[@inline] find_assignment state reg = Reg.Map.find_opt reg state.assignments

let[@inline] clear_assignments state = state.assignments <- Reg.Map.empty

let[@inline] add_introduced_temporaries_list state l =
  state.introduced_temporaries
    <- List.fold_left l ~init:state.introduced_temporaries ~f:(fun set reg ->
           Reg.Set.add reg set)

let[@inline] mem_introduced_temporaries state reg =
  Reg.Set.mem reg state.introduced_temporaries

let[@inline] iter_introduced_temporaries state ~f =
  Reg.Set.iter f state.introduced_temporaries

let[@inline] introduced_temporary_count state =
  Reg.Set.cardinal state.introduced_temporaries

let[@inline] initial_temporary_count state = state.initial_temporaries

let[@inline] stack_slots state = state.stack_slots

let[@inline] get_and_incr_instruction_id state =
  InstructionId.get_next state.instruction_id
