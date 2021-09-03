(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The representation of the application of a continuation.  In the
    zero-arity case this is just "goto". *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val create
   : ?trap_action:Trap_action.t
  -> Continuation.t
  -> args:Simple.t list
  -> dbg:Debuginfo.t
  -> t

val goto : Continuation.t -> t

val continuation : t -> Continuation.t

val args : t -> Simple.t list

val trap_action : t -> Trap_action.t option

val debuginfo : t -> Debuginfo.t

(* CR mshinwell: Use "with" not "update" *)
val update_continuation : t -> Continuation.t -> t

val update_continuation_and_args
   : t
  -> Continuation.t
  -> args:Simple.t list
  -> t

val update_args : t -> args:Simple.t list -> t

val with_debuginfo : t -> dbg:Debuginfo.t -> t

val is_goto : t -> bool

val is_goto_to : t -> Continuation.t -> bool

val to_goto : t -> Continuation.t option

val clear_trap_action : t -> t

val to_one_arg_without_trap_action : t -> Simple.t option

include Container_types.S with type t := t
