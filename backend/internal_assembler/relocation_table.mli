type t

val create : string -> t

val make_relocation :
  t ->
  X86_binary_emitter.Relocation.t ->
  Symbol_table.t ->
  String_table.t ->
  unit

val name : t -> string

val num_relocations : t -> int

val write : t -> Section_table.t -> Owee.Owee_buf.t -> unit
