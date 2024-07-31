(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Common functions for emitting assembly code *)

val output_channel : out_channel ref

val emit_string : string -> unit

val emit_int : int -> unit

val emit_nativeint : nativeint -> unit

val emit_int32 : int32 -> unit

val emit_symbol : string -> unit

val emit_printf : ('a, out_channel, unit) format -> 'a

val emit_char : char -> unit

val emit_string_literal : string -> unit

val emit_string_directive : string -> string -> unit

val emit_bytes_directive : string -> string -> unit

val emit_float64_directive : string -> int64 -> unit

val emit_float64_split_directive : string -> int64 -> unit

val emit_float32_directive : string -> int32 -> unit

val reset : unit -> unit

val reset_debug_info : unit -> unit

val emit_debug_info : ?discriminator:int -> Debuginfo.t -> unit

val emit_debug_info_gen :
  ?discriminator:int ->
  Debuginfo.t ->
  (file_num:int -> file_name:string -> unit) ->
  (file_num:int -> line:int -> col:int -> ?discriminator:int -> unit -> unit) ->
  unit

(** Get the file number associated with the filename (or allocate one) *)
val get_file_num :
  file_emitter:(file_num:int -> file_name:string -> unit) -> string -> int

type frame_debuginfo =
  | Dbg_alloc of Debuginfo.alloc_dbginfo
  | Dbg_raise of Debuginfo.t
  | Dbg_other of Debuginfo.t

val record_frame_descr :
  label:int ->
  (* Return address *)
  frame_size:int ->
  (* Size of stack frame *)
  live_offset:int list ->
  (* Offsets/regs of live addresses *)
  frame_debuginfo ->
  (* Location, if any *)
  unit

type emit_frame_actions =
  { efa_code_label : int -> unit;
    efa_data_label : int -> unit;
    efa_8 : int -> unit;
    efa_16 : int -> unit;
    efa_32 : int32 -> unit;
    efa_word : int -> unit;
    efa_align : int -> unit;
    efa_label_rel : int -> int32 -> unit;
    efa_def_label : int -> unit;
    efa_string : string -> unit
  }

val emit_frames : emit_frame_actions -> unit

val is_generic_function : string -> bool

val cfi_startproc : unit -> unit

val cfi_endproc : unit -> unit

val cfi_adjust_cfa_offset : int -> unit

val cfi_offset : reg:int -> offset:int -> unit

val cfi_def_cfa_offset : int -> unit

val cfi_remember_state : unit -> unit

val cfi_restore_state : unit -> unit

val cfi_def_cfa_register : reg:int -> unit

(** Is a binary backend available.  If yes, we don't need
        to generate the textual assembly file (unless the user
        request it with -S). *)
val binary_backend_available : bool ref

(** Clear global state and compact the heap, so that an external program
    (such as the assembler or linker) may have more memory available to it.

    When this frees up around 1.1GB of memory, it takes around 0.6s. We only
    take this time when the job is large enough that we're worried that we'll
    either run out of memory or constrain the number of parallel jobs. We
    heuristically measure how big the job is by how much heap we're using
    ourselves.

    The [reset] parameter will be called before [Gc.compact] if we go ahead
    with the compaction. It should clear as much as possible from the global
    state, since the fewer live words there are after GC, the smaller the new
    heap can be. *)
val reduce_heap_size : reset:(unit -> unit) -> unit

type error =
  | Stack_frame_too_large of int
  | Stack_frame_way_too_large of int
  | Inconsistent_probe_init of string * Debuginfo.t

module Dwarf_helpers : sig
  val init : disable_dwarf:bool -> string -> unit

  val begin_dwarf :
    build_asm_directives:(unit -> (module Asm_targets.Asm_directives_intf.S)) ->
    code_begin:string ->
    code_end:string ->
    file_emitter:(file_num:int -> file_name:string -> unit) ->
    unit

  val emit_dwarf : unit -> unit

  val emit_delayed_dwarf : unit -> unit

  val record_dwarf_for_fundecl : Linear.fundecl -> Dwarf.fundecl option
end

exception Error of error

val report_error : Format.formatter -> error -> unit

val add_stack_checks_if_needed :
  Linear.fundecl ->
  stack_offset:int ->
  stack_threshold_size:int ->
  trap_size:int ->
  Linear.fundecl
