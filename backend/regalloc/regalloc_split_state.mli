[@@@ocaml.warning "+a-4-30-40-41-42"]

open Regalloc_utils
open Regalloc_split_utils

type destructions_at_end = (destruction_kind * Reg.Set.t) Label.Map.t

type definitions_at_beginning = Reg.Set.t Label.Map.t

type phi_at_beginning = Reg.Set.t Label.Map.t

type t

val log_renaming_info : indent:int -> t -> unit

(** Constructs the renaming information necessary to split the live ranges.

   The components are:
   - destructions_at_end, a map from labels (of blocks ending with a
     destruction point) to sets of registers that need to be spilled;
   - definitions_at_beginning, a map from labels (of blocks being
     successors of destruction point) to sets of registers that need to
     be reloaded;
   - phi_at_beginning, a map from labels (of blocks starting with
     phi functions) to sets of registers that need to be unified
     (this set contains the original name of the registers which need
      a phi, i.e. if we need to insert `x = phi(x', x'')` then the set
      contains `x`). *)
val make : Cfg_with_infos.t -> next_instruction_id:Instruction.id -> t

val destructions_at_end : t -> destructions_at_end

val definitions_at_beginning : t -> definitions_at_beginning

val phi_at_beginning : t -> phi_at_beginning

val stack_slots : t -> Regalloc_stack_slots.t

val get_and_incr_instruction_id : t -> Instruction.id
