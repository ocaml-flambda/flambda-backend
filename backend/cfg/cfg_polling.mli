[@@@ocaml.warning "+a-30-40-41-42"]

val instrument_fundecl : future_funcnames:Misc.Stdlib.String.Set.t
  -> Cfg_with_layout.t -> Cfg_with_layout.t

val requires_prologue_poll : future_funcnames:Misc.Stdlib.String.Set.t
  -> fun_name:string -> Cfg_with_layout.t -> bool
