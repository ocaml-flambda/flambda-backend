type t

val create : unit -> t

val add_symbol : t -> Symbol_entry.t -> unit

val add_label : t -> X86_binary_emitter.symbol -> Symbol_entry.t -> unit

val get_label :
  t ->
  X86_binary_emitter.StringTbl.key ->
  X86_binary_emitter.symbol * Symbol_entry.t

val get_label_idx :
  t -> X86_binary_emitter.StringTbl.key -> X86_binary_emitter.symbol * int

val get_symbol_idx_opt : t -> X86_binary_emitter.StringTbl.key -> int option

val num_symbols : t -> int

val num_locals : t -> int

val make_undef_symbol : t -> string -> String_table.t -> unit

val make_section_symbol : t -> Owee.Owee_buf.u16 -> 'a -> Symbol_entry.t

val make_symbol :
  t -> X86_binary_emitter.symbol -> Section_table.t -> String_table.t -> unit

val write : t -> int64 -> Owee.Owee_buf.t -> unit
