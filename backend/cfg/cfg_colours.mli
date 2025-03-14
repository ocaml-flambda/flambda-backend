(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A colour directive. Can be passed as an argument to [Format.printf] and
    frients using the "%t" specifier. Each directive (besides [pop]) acts by
    pushing a new state onto a stack, allowing the previous state to be restored
    using [pop]. *)
type directive = Format.formatter -> unit

(** Undo the most recent colour directive, restoring the previous state. Raises
    a fatal error if the stack is empty. *)
val pop : directive

(** Push a copy of the current state onto the stack. Useful when setting a
    colour conditionally so that a following [pop] will always be matched. *)
val none : directive

val terminator : directive

val block_label : directive

val block_label_exn : directive

val instr_id : directive

val pred_succ : directive

val function_name : directive

val operation : directive

val reg_stamp : directive

val without_colours : f:(unit -> 'a) -> 'a
