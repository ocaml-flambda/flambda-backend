[@@@ocaml.warning "+a-30-40-41-42"]

module Array : module type of ArrayLabels

module List : module type of ListLabels

val on_fatal : f:(unit -> unit) -> unit

val fatal : ('a, Format.formatter, unit, 'b) format4 -> 'a

val find_param_value : string -> string option

val debug : bool

val bool_of_param :
  ?guard:bool * string -> ?default:bool -> string -> bool Lazy.t

val invariants : bool Lazy.t

val verbose : bool Lazy.t

val validator_debug : bool Lazy.t

val block_temporaries : bool Lazy.t

type liveness = Cfg_with_infos.liveness

type log_function =
  { indent : unit -> unit;
    dedent : unit -> unit;
    reset_indentation : unit -> unit;
    log : 'a. ?no_eol:unit -> ('a, Format.formatter, unit) format -> 'a;
    enabled : bool
  }

val make_log_function : label:string -> log_function

module Instruction : sig
  type id = InstructionId.t

  type t = Cfg.basic Cfg.instruction

  val dummy : t

  val compare : t -> t -> int

  module Set : Set.S with type elt = t

  module IdSet : MoreLabels.Set.S with type elt = id

  module IdMap : MoreLabels.Map.S with type key = id
end

val first_instruction_id : Cfg.basic_block -> InstructionId.t

type cfg_infos =
  { arg : Reg.Set.t;
    res : Reg.Set.t;
    max_instruction_id : InstructionId.t
  }

(* CR xclerc for xclerc: this function currently reset the worklist to
   "unknown", but that should be done elsewhere as functions in this module are
   supposed to be independent of IRC. note: we are also copying arg/res for all
   instructions, to break sharing. *)
val collect_cfg_infos : Cfg_with_layout.t -> cfg_infos

val make_log_body_and_terminator :
  log_function ->
  instr_prefix:(Cfg.basic Cfg.instruction -> string) ->
  term_prefix:(Cfg.terminator Cfg.instruction -> string) ->
  Cfg.basic_instruction_list ->
  Cfg.terminator Cfg.instruction ->
  liveness ->
  unit

val make_log_cfg_with_infos :
  log_function ->
  instr_prefix:(Cfg.basic Cfg.instruction -> string) ->
  term_prefix:(Cfg.terminator Cfg.instruction -> string) ->
  Cfg_with_infos.t ->
  unit

module Move : sig
  type t =
    | Plain
    | Load
    | Store

  val make_instr :
    t ->
    id:InstructionId.t ->
    copy:_ Cfg.instruction ->
    from:Reg.t ->
    to_:Reg.t ->
    Instruction.t

  val to_string : t -> string
end

val same_reg_class : Reg.t -> Reg.t -> bool

val same_stack_class : Reg.t -> Reg.t -> bool

val simplify_cfg : Cfg_with_layout.t -> Cfg_with_layout.t

val save_cfg : string -> Cfg_with_layout.t -> unit

val remove_prologue_if_not_required : Cfg_with_layout.t -> unit

val update_live_fields : Cfg_with_layout.t -> liveness -> unit

module SpillCosts : sig
  type t

  val empty : unit -> t

  val iter : t -> f:(Reg.t -> int -> unit) -> unit

  val for_reg : t -> Reg.t -> int

  val add_to_reg : t -> Reg.t -> int -> unit

  (* The spill cost is currently the number of occurrences of the register. If
     [flat] is true, the same weight is given to all uses; if [flat] is false,
     the information about loops is computed and used to give more weight to
     uses inside (nested) loops. *)
  val compute : Cfg_with_infos.t -> flat:bool -> unit -> t
end

val check_length : string -> 'a array -> int -> unit

val check_lengths : of_arg:int -> of_res:int -> 'a Cfg.instruction -> unit

val check_same : string -> Reg.t -> string -> Reg.t -> unit

(* CR xclerc for xclerc: consider renaming the constructors *)
type stack_operands_rewrite =
  | All_spilled_registers_rewritten
  | May_still_have_spilled_registers

val equal_stack_operands_rewrite :
  stack_operands_rewrite -> stack_operands_rewrite -> bool

(* Substitution/map from registers to their spilled counterparts. *)
type spilled_map = Regalloc_substitution.t

val is_spilled : spilled_map -> Reg.t -> bool

val use_stack_operand : spilled_map -> Reg.t array -> int -> unit

val may_use_stack_operands_array : spilled_map -> Reg.t array -> unit

val may_use_stack_operands_everywhere :
  spilled_map -> 'a Cfg.instruction -> stack_operands_rewrite

val occurs_array : Reg.t array -> Reg.t -> bool

val occurs_instruction : _ Cfg.instruction -> Reg.t -> bool

val occurs_block_body : Cfg.basic_block -> Reg.t -> bool

val occurs_block : Cfg.basic_block -> Reg.t -> bool
