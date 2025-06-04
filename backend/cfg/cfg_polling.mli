[@@@ocaml.warning "+a-40-41-42"]

(** Analyses related to the insertion of [Poll] operations. *)

val instrument_fundecl :
  future_funcnames:Misc.Stdlib.String.Set.t ->
  Cfg_with_layout.t ->
  Cfg_with_layout.t

val requires_prologue_poll :
  future_funcnames:Misc.Stdlib.String.Set.t ->
  fun_name:string ->
  optimistic_prologue_poll_instr_id:InstructionId.t ->
  Cfg.t ->
  bool
