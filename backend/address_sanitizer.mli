type memory_access =
  | Load
  | Store

val max_supported_log2size : int

val is_enabled : unit -> bool

val command_line_options : (string * Arg.spec * string) list

(** Performs the check described by [https://github.com/google/sanitizers/wiki/AddressSanitizerAlgorithm]
    for a memory access of size [log2size] on the address that the given expression evaluates to. *)
val check :
  memory_access ->
  log2size:int ->
  Cmm.expression ->
  Debuginfo.t ->
  Cmm.expression
