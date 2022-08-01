type t

type bind =
  | Local
  | Global

type symbol_type =
  | NoType
  | Object
  | Func
  | Section
  | File
  | Common
  | Tls

val create_undef_symbol : string -> String_table.t -> t

val create_section_symbol : Owee.Owee_buf.u16 -> t

val create_symbol :
  X86_binary_emitter.symbol -> 'a -> Section_table.t -> String_table.t -> t

val create_got_symbol : 'a -> String_table.t -> t

val get_bind : t -> bind

val get_type : t -> symbol_type

val get_shndx : t -> Owee.Owee_buf.u16

val get_name_str : t -> string

val write : t -> Owee__Owee_buf.cursor -> unit
