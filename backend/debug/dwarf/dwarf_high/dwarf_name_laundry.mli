(** The name laundry: where names get (de)mangled. (This functionality is used
    by the debugger support library as well as the compiler.) *)

val mangle_linker_dirs : string list -> string

val demangle_linker_dirs : string -> string list
