type memory_access =
  | Load
  | Store

val max_supported_log2size : int

(** Performs the check described by [https://github.com/google/sanitizers/wiki/AddressSanitizerAlgorithm]
    for a memory access of size [log2size] on the address that the given expression evaluates to. *)
val check :
  memory_access ->
  log2size:int ->
  Cmm.expression ->
  Debuginfo.t ->
  Cmm.expression
