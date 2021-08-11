type dwarf_section =
  | Debug_info
  | Debug_abbrev
  | Debug_aranges
  | Debug_loc
  | Debug_str
  | Debug_line

type power_section =
  | Function_descriptors
  | Table_of_contents

type ia32_section =
  | Non_lazy_symbol_pointers
  | Jump_table

type section =
  | Text
  | Data
  | Read_only_data
  | Eight_byte_literals
  | Sixteen_byte_literals
  | Jump_tables
  | DWARF of dwarf_section
  | POWER of power_section
  | IA32 of ia32_section

let switch_to_section _section = failwith "not implemented"

let int8 _num = failwith "not implemented"

let int16 _num = failwith "not implemented"

let int32 _num = failwith "not implemented"

let int64 _num = failwith "not implemented"

let targetint _num = failwith "not implemented"

let uleb128 _num = failwith "not implemented"

let sleb128 _num = failwith "not implemented"

let float32 _num = failwith "not implemented"

let float64 _num = failwith "not implemented"

let float64_from_bits _num = failwith "not implemented"

let string _arg = failwith "not implemented"

let cache_string _string = failwith "not implemented"

(** Emit the sequence of:
      label definition:
        <string><null terminator>
    pairs as per previous calls to [cache_string].  This function clears
    the cache. *)
let emit_cached_strings : unit -> unit = failwith "not implemented"

let comment : string -> unit = failwith "not implemented"

let file : file_num:int -> file_name:string -> unit = failwith "not implemented"

let loc : file_num:int -> line:int -> col:int -> unit = failwith "not implemented"

let cfi_adjust_cfa_offset : bytes:int -> unit = failwith "not implemented"

let cfi_offset : reg:int -> offset:int -> unit = failwith "not implemented"

let cfi_startproc : unit -> unit = failwith "not implemented"

let cfi_endproc : unit -> unit = failwith "not implemented"

let mark_stack_non_executable : unit -> unit = failwith "not implemented"

let align : bytes:int -> unit = failwith "not implemented"

let size : ?size_of_symbol:string -> symbol:string -> unit = failwith "not implemented"

let space : bytes:int -> unit = failwith "not implemented"

let define_data_symbol : symbol:string -> unit = failwith "not implemented"

let define_function_symbol : symbol:string -> unit = failwith "not implemented"

let global : symbol:string -> unit = failwith "not implemented"

let symbol : string -> unit = failwith "not implemented"

let private_extern : symbol:string -> unit = failwith "not implemented"

let indirect_symbol : symbol:string -> unit = failwith "not implemented"

let define_label : Dwarf_misc.cmm_label -> unit = failwith "not implemented"

let label : Dwarf_misc.cmm_label -> unit = failwith "not implemented"

let symbol_plus_offset : symbol:string -> offset_in_bytes:Targetint.t -> unit = failwith "not implemented"

let between_symbols : upper:string -> lower:string -> unit = failwith "not implemented"

let between_labels_32bit : upper:Dwarf_misc.cmm_label -> lower:Dwarf_misc.cmm_label -> unit = failwith "not implemented"

let between_symbol_and_label_offset : upper:Dwarf_misc.cmm_label -> lower:string -> offset_upper:Targetint.t -> unit = failwith "not implemented"

let between_symbol_and_label_offset' : upper:string -> lower:Dwarf_misc.cmm_label -> offset_lower:Targetint.t -> unit = failwith "not implemented"

let between_this_and_label_offset_32bit : upper:Dwarf_misc.cmm_label -> offset_upper:Targetint.t -> unit = failwith "not implemented"

let scaled_distance_between_this_and_label_offset : upper:Dwarf_misc.cmm_label -> divide_by:int -> unit = failwith "not implemented"

let offset_into_section_label : section:section -> label:Dwarf_misc.cmm_label -> width:Target_system.machine_width -> unit = failwith "not implemented"

let offset_into_section_symbol : section:section -> symbol:string -> width:Target_system.machine_width -> unit = failwith "not implemented"

let label_for_section : section -> Dwarf_misc.cmm_label = failwith "not implemented"

module Directive = struct
  module Constant = struct
    type t = private
      | Signed_int of Int64.t
      | This
      | Named_thing of string
      | Add of t * t
      | Sub of t * t
      | Div of t * int
  end

  module Constant_with_width = struct
    type t = unit

    let constant : t -> Constant.t = failwith "not implemented"

    type width_in_bytes = private
      | Eight
      | Sixteen
      | Thirty_two
      | Sixty_four

    let width_in_bytes : t -> width_in_bytes = failwith "not implemented"
  end

  type thing_after_label = private
    | Code
    | Machine_width_data

  type comment = private string

  type t = private
    | Align of { bytes : int; }
    | Bytes of string
    | Cfi_adjust_cfa_offset of int
    | Cfi_endproc
    | Cfi_offset of { reg : int; offset : int; }
    | Cfi_startproc
    | Comment of comment
    | Const of { constant : Constant_with_width.t; comment : string option; }
    | Direct_assignment of string * Constant.t
    | File of { file_num : int option; filename : string; }
    | Global of string
    | Indirect_symbol of string
    | Loc of { file_num : int; line : int; col : int; }
    | New_label of string * thing_after_label
    | Private_extern of string
    | Section of {
        names : string list;
        flags : string option;
        args : string list;
      }
    | Size of string * Constant.t
    | Sleb128 of Constant.t
    | Space of { bytes : int; }
    | Type of string * string
    | Uleb128 of Constant.t

  let print : Buffer.t -> t -> unit = failwith "not implemented"
end

let initialize : emit:(Directive.t -> unit) -> unit = failwith "not implemented"

let reset : unit -> unit = failwith "not implemented"

let string_of_label : Dwarf_misc.cmm_label -> string = failwith "not implemented"