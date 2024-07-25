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

(** The representation of the application of an OCaml function, OCaml method or
    external call to a list of arguments. *)

type t

val free_names_except_callee : t -> Name_occurrences.t

include Expr_std.S with type t := t

val free_names_without_exn_continuation : t -> Name_occurrences.t

include Contains_ids.S with type t := t

module Result_continuation : sig
  type t =
    | Return of Continuation.t
    | Never_returns

  include Container_types.S with type t := t

  include Contains_names.S with type t := t
end

module Position : sig
  type t =
    | Normal
    | Nontail

  val equal : t -> t -> bool
end

(** Create an application expression. *)
val create :
  callee:Simple.t option ->
  continuation:Result_continuation.t ->
  Exn_continuation.t ->
  args:Simple.t list ->
  args_arity:[`Complex] Flambda_arity.t ->
  return_arity:[`Unarized] Flambda_arity.t ->
  call_kind:Call_kind.t ->
  Debuginfo.t ->
  inlined:Inlined_attribute.t ->
  inlining_state:Inlining_state.t ->
  probe:Probe.t ->
  position:Position.t ->
  original_position:Original_position.t ->
  relative_history:Inlining_history.Relative.t ->
  t

(* CR mshinwell: This doesn't really make sense for C calls; we should have a
   separate type of symbols for those too, since [Symbol.t] is for data
   symbols. *)

(* CR mshinwell: Try to have a more robust way of tracking applications of
   probes *)

(** The function or method being applied. *)
val callee : t -> Simple.t option

(** The arguments of the function or method being applied. *)
val args : t -> Simple.t list

(** The arity of the arguments being applied. *)
val args_arity : t -> [`Complex] Flambda_arity.t

(** The arity of the result(s) of the application. *)
val return_arity : t -> [`Unarized] Flambda_arity.t

(** Information about what kind of call is involved (direct function call,
    method call, etc). *)
val call_kind : t -> Call_kind.t

(** Where to send the result of the application. *)
val continuation : t -> Result_continuation.t

(** Where to jump to upon the application raising an exception. *)
val exn_continuation : t -> Exn_continuation.t

(** Debugging information attached to the application. *)
val dbg : t -> Debuginfo.t

(** Instructions from the source code as to whether the callee should be
    inlined. *)
val inlined : t -> Inlined_attribute.t

(** Whether the call was marked [@nontail] *)
val position : t -> Position.t

val erase_callee : t -> t

(** Change the return continuation of an application. *)
val with_continuation : t -> Result_continuation.t -> t

val with_continuations : t -> Result_continuation.t -> Exn_continuation.t -> t

val with_exn_continuation : t -> Exn_continuation.t -> t

(** Change the arguments of an application *)
val with_args : t -> Simple.t list -> args_arity:[`Complex] Flambda_arity.t -> t

(** Change the call kind of an application. *)
val with_call_kind : t -> Call_kind.t -> t

val inlining_state : t -> Inlining_state.t

val inlining_arguments : t -> Inlining_arguments.t

val probe : t -> Probe.t

val original_position : t -> Original_position.t

val relative_history : t -> Inlining_history.Relative.t

(** Returns [true] if the application returns to the caller, [false] if it is
    non terminating. *)
val returns : t -> bool

val with_inlined_attribute : t -> Inlined_attribute.t -> t
