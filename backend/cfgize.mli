val fundecl
  : Mach.fundecl
  -> preserve_orig_labels:bool
  -> prologue_required:bool
  -> dbg:Debuginfo.t
  -> fdo:Fdo_info.t
  -> Cfg_with_layout.t
