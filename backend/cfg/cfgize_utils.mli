val terminator_of_test :
  Simple_operation.test ->
  label_false:Label.t ->
  label_true:Label.t ->
  Cfg.terminator

val invalid_stack_offset : int

module Stack_offset_and_exn : sig
  val update_cfg : Cfg.t -> unit
end
