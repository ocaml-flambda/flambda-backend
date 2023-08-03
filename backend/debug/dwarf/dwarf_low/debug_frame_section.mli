(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Oscar Hill, Jane Street Europe                      *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Representation of the DWARF .debug_frame section. *)

open Asm_targets

type t

val create : code_begin:Asm_symbol.t -> t

val process_cfi_startproc : t -> address:int -> unit

val process_cfi_adjust_cfa_offset : t -> address:int -> offset:int -> unit

val process_cfi_endproc : t -> address:int -> unit

val checkpoint : t -> unit

val rollback : t -> unit

include Dwarf_emittable.S with type t := t
