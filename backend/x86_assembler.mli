(* module StringTbl : Hashtbl.S with type key := string *)

(* module IntTbl : Hashtbl.S with type key := int *)

module StringTable : sig
  type t

  val create : unit -> t

  val add_string : t -> string -> unit

  val current_length : t -> int

  val get_strings : t -> string list
end

module SymbolEntry : sig
  type t
  val create_undef_symbol : string -> StringTable.t -> t
  val create_section_symbol : Owee.Owee_buf.u16 -> t
end

module SymbolTable : sig
  type t

  val create : unit -> t

  val add_symbol : t -> SymbolEntry.t -> unit

  val add_label : t -> X86_binary_emitter.symbol -> SymbolEntry.t -> unit

  val get_label_idx :
    t -> X86_binary_emitter.StringMap.key -> X86_binary_emitter.symbol * int

  val get_symbol_idx_opt : t -> string -> int option

  val num_symbols : t -> int

  (* val finalise : t -> unit *)

  val num_locals : t -> int

  val get_symbols : t -> SymbolEntry.t list
end 

val assemble : X86_ast.asm_line list -> string -> unit
