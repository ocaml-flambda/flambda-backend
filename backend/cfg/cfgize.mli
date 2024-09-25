val terminator_of_test :
  Mach.test -> label_false:Label.t -> label_true:Label.t -> Cfg.terminator

val fundecl :
  Mach.fundecl ->
  before_register_allocation:bool ->
  preserve_orig_labels:bool ->
  simplify_terminators:bool ->
  Cfg_with_layout.t
