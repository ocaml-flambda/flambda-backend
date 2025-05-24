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

[@@@ocaml.warning "+a-40-41-42"]

val output_channel : out_channel ref

val emit_string : string -> unit

val emit_buffer : Buffer.t -> unit

val reset : unit -> unit

val reset_debug_info : unit -> unit

val emit_debug_info_gen :
  ?discriminator:int ->
  Debuginfo.t ->
  (file_num:int -> file_name:string -> unit) ->
  (file_num:int -> line:int -> col:int -> ?discriminator:int -> unit -> unit) ->
  unit

type frame_debuginfo =
  | Dbg_alloc of Cmm.alloc_dbginfo
  | Dbg_raise of Debuginfo.t
  | Dbg_other of Debuginfo.t

val record_frame_descr :
  label:Label.t ->
  (* Return address *)
  frame_size:int ->
  (* Size of stack frame *)
  live_offset:int list ->
  (* Offsets/regs of live addresses *)
  frame_debuginfo ->
  (* Location, if any *)
  unit

type emit_frame_actions =
  { efa_code_label : Label.t -> unit;
    efa_data_label : Label.t -> unit;
    efa_i8 : Numbers.Int8.t -> unit;
    efa_i16 : Numbers.Int16.t -> unit;
    efa_i32 : Int32.t -> unit;
    efa_u8 : Numbers.Uint8.t -> unit;
    efa_u16 : Numbers.Uint16.t -> unit;
    efa_u32 : Numbers.Uint32.t -> unit;
    efa_word : int -> unit;
    efa_align : int -> unit;
    efa_label_rel : Label.t -> int32 -> unit;
    efa_def_label : Label.t -> unit;
    efa_string : string -> unit
  }

val emit_frames : emit_frame_actions -> unit

val is_generic_function : string -> bool

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
  val init : disable_dwarf:bool -> sourcefile:string option -> unit

  val begin_dwarf :
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

type preproc_stack_check_result =
  { max_frame_size : int;
    contains_nontail_calls : bool
  }

val preproc_stack_check :
  fun_body:Linear.instruction ->
  frame_size:int ->
  trap_size:int ->
  preproc_stack_check_result

val add_stack_checks_if_needed :
  Linear.fundecl ->
  stack_offset:int ->
  stack_threshold_size:int ->
  trap_size:int ->
  Linear.fundecl
