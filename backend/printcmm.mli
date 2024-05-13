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

(* Pretty-printing of C-- code *)

open Format

val symbol : formatter -> Cmm.symbol -> unit
val rec_flag : formatter -> Cmm.rec_flag -> unit
val machtype_component : formatter -> Cmm.machtype_component -> unit
val machtype : formatter -> Cmm.machtype -> unit
val exttype : formatter -> Cmm.exttype -> unit
val extcall_signature : formatter -> Cmm.machtype option * Cmm.exttype list -> unit
val integer_comparison : Cmm.integer_comparison -> string
val float_comparison : Cmm.float_comparison -> string
val trap_action_list : formatter -> Cmm.trap_action list -> unit
val chunk : Cmm.memory_chunk -> string
val atomic_bitwidth : Cmm.atomic_bitwidth -> string
val operation : Debuginfo.t -> Cmm.operation -> string
val expression : formatter -> Cmm.expression -> unit
val fundecl : formatter -> Cmm.fundecl -> unit
val data : formatter -> Cmm.data_item list -> unit
val phrase : formatter -> Cmm.phrase -> unit
val temporal_locality : Cmm.prefetch_temporal_locality_hint -> string
val print_codegen_options : formatter -> Cmm.codegen_option list -> unit
