(** A "sub" CFG is the counterpart of an instruction list in the original Mach
    selection pass.

    It is essentially a collection of blocks (stored as a layout, i.e. as a
    list), with two designated blocks:

    - an entry block;

    - an exit block.

    The exit block is where more instructions are being added, which means that
    the terminator of an in-construction "sub" CFG is `Never`, and will be
    changed only when no additional instructions will be inserted to the
    block. *)

type t

val exit_has_never_terminator : t -> bool

val make_empty : unit -> t

val add_empty_block_at_start : t -> label:Label.t -> t

val add_never_block : t -> label:Label.t -> t

(** Use [add_instruction] in preference to this function. *)
val add_instruction_at_start :
  t -> Cfg.basic -> Reg.t array -> Reg.t array -> Debuginfo.t -> unit

(** [add_instruction] can only be called when the terminator is [Never]. *)
val add_instruction :
  t -> Cfg.basic -> Reg.t array -> Reg.t array -> Debuginfo.t -> unit

(** [add_instruction'] can only be called when the terminator is [Never]. *)
val add_instruction' : t -> Cfg.basic Cfg.instruction -> unit

(** [set_terminator] can only be called when the terminator is [Never]. *)
val set_terminator :
  t -> Cfg.terminator -> Reg.t array -> Reg.t array -> Debuginfo.t -> unit

val iter_basic_blocks : t -> f:(Cfg.basic_block -> unit) -> unit

val exists_basic_blocks : t -> f:(Cfg.basic_block -> bool) -> bool

val join : from:t list -> to_:t -> t

val join_tail : from:t list -> to_:t -> t

val update_exit_terminator : ?arg:Reg.t array -> t -> Cfg.terminator -> unit

val start_label : t -> Label.t

val mark_as_trap_handler : t -> exn_label:Label.t -> unit

val dump : t -> unit
