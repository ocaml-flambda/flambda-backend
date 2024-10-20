val terminator_of_test :
  Mach.test -> label_false:Label.t -> label_true:Label.t -> Cfg.terminator

module Stack_offset_and_exn : sig
  val update_cfg : Cfg.t -> unit
end

val fundecl :
  Mach.fundecl ->
  before_register_allocation:bool ->
  preserve_orig_labels:bool ->
  simplify_terminators:bool ->
  Cfg_with_layout.t
