type fundecl = { fun_name : string }

val for_fundecl : params:(module Dwarf_params.S) -> Dwarf_state.t -> fundecl -> unit