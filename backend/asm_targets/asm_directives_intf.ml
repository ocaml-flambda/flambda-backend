(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2014-2022 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
module type Arg = sig
  val emit_line : string -> unit

  (* Should be Emitaux.get_file_num *)
  val get_file_num : string -> int

  val debugging_comments_in_asm_files : bool

  module D : sig
    type constant

    val const_int64 : int64 -> constant

    val const_label : string -> constant

    val const_add : constant -> constant -> constant

    val const_sub : constant -> constant -> constant

    type data_type =
      | NONE
      | DWORD
      | QWORD
      | VEC128

    val file : file_num:int -> file_name:string -> unit

    val loc :
      file_num:int -> line:int -> col:int -> ?discriminator:int -> unit -> unit

    val comment : string -> unit

    val label : ?data_type:data_type -> string -> unit

    val section :
      ?delayed:bool -> string list -> string option -> string list -> unit

    val text : unit -> unit

    val new_line : unit -> unit

    val global : protected:bool -> string -> unit

    val type_ : string -> string -> unit

    val byte : constant -> unit

    val word : constant -> unit

    val long : constant -> unit

    val qword : constant -> unit

    val bytes : string -> unit

    val uleb128 : constant -> unit

    val sleb128 : constant -> unit

    val direct_assignment : string -> constant -> unit
  end
end

module type S = sig
  (** Emit subsequent directives to the given section. If this function has not
      been called before on the particular section, a label declaration will be
      emitted after declaring the section. Such labels may seem strange, but
      they are necessary so that references (e.g. DW_FORM_ref_addr /
      DW_FORM_sec_offset when emitting DWARF) to places that are currently at
      the start of these sections get relocated correctly when those places
      become not at the start (e.g. during linking).
  *)
  val switch_to_section : Asm_section.t -> unit

  (** Called at the beginning of the assembly generation and only if the dwarf
      flag has been set. *)
  val initialize : unit -> unit

  (** Emit an 8-bit signed integer. There is no padding or sign extension. If
      the [comment] is specified it will be put on the same line as the
      integer. *)
  val int8 : ?comment:string -> Numbers.Int8.t -> unit

  (** Emit a 16-bit signed integer. There is no padding or sign extension. *)
  val int16 : ?comment:string -> Numbers.Int16.t -> unit

  (** Emit a 32-bit signed integer. There is no padding or sign extension. *)
  val int32 : ?comment:string -> Int32.t -> unit

  (** Emit a 64-bit signed integer. *)
  val int64 : ?comment:string -> Int64.t -> unit

  (** Emit an 8-bit unsigned integer. There is no padding. *)
  val uint8 : ?comment:string -> Numbers.Uint8.t -> unit

  (** Emit a 16-bit unsigned integer. There is no padding. *)
  val uint16 : ?comment:string -> Numbers.Uint16.t -> unit

  (** Emit a 32-bit unsigned integer. There is no padding. *)
  val uint32 : ?comment:string -> Numbers.Uint32.t -> unit

  (** Emit a 64-bit unsigned integer. There is no padding. *)
  val uint64 : ?comment:string -> Numbers.Uint64.t -> unit

  (* CR-soon mshinwell: Target addresses should not be signed *)

  (** Emit a signed integer whose width is that of an address on the target
      machine. There is no padding or sign extension. *)
  val targetint : ?comment:string -> Targetint.t -> unit

  (** Emit a 64-bit integer in unsigned LEB128 variable-length encoding (cf.
      DWARF debugging information standard). *)
  val uleb128 : ?comment:string -> Numbers.Uint64.t -> unit

  (** Emit a 64-bit integer in signed LEB128 variable-length encoding. *)
  val sleb128 : ?comment:string -> Int64.t -> unit

  (** Emit a string (directly into the current section). This function does not
      write a terminating null. *)
  val string : ?comment:string -> string -> unit

  (** Cache a string for later emission. The returned label may be used to
      obtain the address of the string in the section. This function does not
      emit anything. (See [emit_cached_strings], below.) If a string is supplied
      to this function that is already in the cache (associated with the same
      section and comment) then the previously-assigned label is returned, not a
      new one. *)
  val cache_string : ?comment:string -> Asm_section.t -> string -> Asm_label.t

  (** Emit the sequence of: label definition: <string><null terminator> pairs as
      per previous calls to [cache_string] with appropriate directives to switch
      section interspersed. This function clears the cache. *)
  val emit_cached_strings : unit -> unit

  (** Emit a comment. *)
  val comment : string -> unit

  (** Emit a blank line. *)
  val new_line : unit -> unit

  (** Define a data ("object") symbol at the current output position. When
      emitting for MASM this will cause loads and stores to/from the symbol to
      be treated as if they are loading machine-width words (unless the
      instruction has an explicit width suffix). *)
  val define_data_symbol : Asm_symbol.t -> unit

  (** Mark a symbol as global. *)
  val global : protected:bool -> Asm_symbol.t -> unit

  (** Emit a machine-width reference to the given symbol. *)
  val symbol : ?comment:string -> Asm_symbol.t -> unit

  (** Define a label at the current position in the current section. The
      treatment for MASM when emitting into non-text sections is as for
      [define_data_symbol], above. *)
  val define_label : Asm_label.t -> unit

  (** Emit a machine-width reference to the given label. *)
  val label : ?comment:string -> Asm_label.t -> unit

  (** Emit a machine-width reference to the address formed by adding the given
      byte offset to the address of the given symbol. The symbol may be in a
      compilation unit and/or section different from the current one. *)
  val symbol_plus_offset : Asm_symbol.t -> offset_in_bytes:Targetint.t -> unit

  (** Emit a machine-width reference giving the displacement between two given
      symbols. To obtain a positive result the symbol at the [lower] address
      should be the second argument, as for normal subtraction. The symbols must
      be in the current compilation unit and in the same section. *)
  val between_symbols_in_current_unit :
    upper:Asm_symbol.t -> lower:Asm_symbol.t -> unit

  (** Like [between_symbols_in_current_unit], but for two labels, emitting a
      16-bit-wide reference. The behaviour upon overflow is unspecified. The
      labels must be in the same section. *)
  val between_labels_16_bit :
    ?comment:string -> upper:Asm_label.t -> lower:Asm_label.t -> unit -> unit

  (** Like [between_symbols_in_current_unit], but for two labels, emitting a
      32-bit-wide reference. The behaviour upon overflow is unspecified. The
      labels must be in the same section. *)
  val between_labels_32_bit :
    ?comment:string -> upper:Asm_label.t -> lower:Asm_label.t -> unit -> unit

  (** Like [between_symbols_in_current_unit], but for two labels, emitting a
      64-bit-wide reference. The labels must be in the same section. *)
  val between_labels_64_bit :
    ?comment:string -> upper:Asm_label.t -> lower:Asm_label.t -> unit -> unit

  (** Emit a machine-width reference giving the displacement between the [lower]
      symbol and the sum of the address of the [upper] label plus
      [offset_upper]. The [lower] symbol must be in the current compilation
      unit. The [upper] label must be in the same section as the [lower]
      symbol. *)
  val between_symbol_in_current_unit_and_label_offset :
    ?comment:string ->
    upper:Asm_label.t ->
    lower:Asm_symbol.t ->
    offset_upper:Targetint.t ->
    unit ->
    unit

  (** Emit an offset into a DWARF section given a label identifying the place
      within such section. *)
  val offset_into_dwarf_section_label :
    ?comment:string ->
    width:Dwarf_flags.dwarf_format ->
    Asm_section.dwarf_section ->
    Asm_label.t ->
    unit

  (** Emit an offset into a DWARF section given a symbol identifying the place
      within such section. The symbol may only be in a compilation unit
      different from the current one if the supplied section is [Debug_info].
      The symbol must always be in the given section. *)
  val offset_into_dwarf_section_symbol :
    ?comment:string ->
    width:Dwarf_flags.dwarf_format ->
    Asm_section.dwarf_section ->
    Asm_symbol.t ->
    unit
end
