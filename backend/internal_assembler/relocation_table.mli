type t

val create : X86_proc.Section_name.t -> t

val make_relocation :
  t ->
  X86_binary_emitter.Relocation.t ->
  Symbol_table.t ->
  String_table.t ->
  unit

val num_relocations : t -> int

val section_name : t -> X86_proc.Section_name.t

val write : t -> Section_table.t -> Owee.Owee_buf.t -> unit
