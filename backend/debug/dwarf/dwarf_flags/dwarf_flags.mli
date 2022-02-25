type debug_thing =
  | Debug_ocamldebug
  | Debug_js_of_ocaml
  | Debug_subprocs
  | Debug_backtraces
  | Debug_bounds_checking
  | Debug_disable_bytecode_opt
  | Debug_dwarf_cfi
  | Debug_dwarf_loc
  | Debug_dwarf_functions
  | Debug_dwarf_scopes
  | Debug_dwarf_vars
  | Debug_dwarf_call_sites
  | Debug_dwarf_cmm

val debug_thing : debug_thing -> bool

val set_debug_thing : debug_thing -> unit

val clear_debug_thing : debug_thing -> unit

val describe_debug_default : debug_thing -> string

val describe_debug_default_negated : debug_thing -> string

val use_g : unit -> unit

val use_g0 : unit -> unit

val use_g1 : unit -> unit

val use_g2 : unit -> unit

val use_g3 : unit -> unit

type dwarf_version =
  | Four
  | Five

val gdwarf_version : dwarf_version ref

val default_gdwarf_version : dwarf_version

val gdwarf_offsets : bool ref

val default_gdwarf_offsets : bool

val gdwarf_self_tail_calls : bool ref

val default_gdwarf_self_tail_calls : bool

type dwarf_format =
  | Thirty_two
  | Sixty_four

val gdwarf_format : dwarf_format ref

val default_gdwarf_format : dwarf_format

val default_ddebug_invariants : bool

val ddebug_invariants : bool ref
