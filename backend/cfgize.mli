val fundecl
  : Mach.fundecl
  -> preserve_orig_labels:bool
  -> prologue_required:(Debuginfo.t * Fdo_info.t) option
  -> Cfg_with_layout.t
