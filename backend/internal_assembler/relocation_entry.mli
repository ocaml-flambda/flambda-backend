type t

val create_relocation :
  X86_binary_emitter.Relocation.t -> Symbol_table.t -> String_table.t -> t

val write : t -> Owee__Owee_buf.cursor -> unit
