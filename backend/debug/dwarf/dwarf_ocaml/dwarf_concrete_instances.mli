type fundecl =
  { fun_name : string
  ; fun_dbg : Debuginfo.t
  }

val for_fundecl : Dwarf_state.t -> fundecl -> unit

(** End symbol name given start symbol name for a function block *)
val end_symbol_name : string -> string