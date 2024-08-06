val cfg :
  Cfg_with_layout.t ->
  Cfg_with_layout.t

val dump : Format.formatter -> Cfg_with_layout.t -> msg:string -> unit
