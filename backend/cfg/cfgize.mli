val fundecl :
  Mach.fundecl ->
  before_register_allocation:bool ->
  preserve_orig_labels:bool ->
  simplify_terminators:bool ->
  Cfg_with_layout.t
