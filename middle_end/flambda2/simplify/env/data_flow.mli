(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Chambart and Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Dataflow analysis.

    This module aims mainly at tracking uses of variables (other things may
    be added later on), with the aim of:
    - removing unused parameters of *recursive* continuations;
    - moving allocations out of the hot path of recursive continuations
      (e.g. the allocation of a float that was unboxed by the simplifier).
*)

type t
(** The type tracking the uses of variables (passed through the downwards
    accumulator).
    This contains a stack to track in which continuation's handler
    the downwards acc currently is. *)

val print : Format.formatter -> t -> unit
(** Print to a formatter. *)


(* {2 Creation and updates} *)

val empty : t
(** Empty uses *)

val init_toplevel : Continuation.t -> Variable.t list -> t -> t
(** Initialize the analysis so that the stack consists of a single
    toplevel continuation. *)

val enter_continuation : Continuation.t -> Variable.t list -> t -> t
(** Add a new continuation on the stack. Used when entering a
    continuation handler. *)

val exit_continuation : Continuation.t -> t -> t
(** Pop the current top of the stack. Used when exiting the current
    continuation handler. *)

val record_binding
   : Variable.t
  -> Name_occurrences.t
  -> generate_phantom_lets:bool
  -> t
  -> t
(** Add a binding from the current handler. This enables the analysis to have
    a fine-grained analysis of dependencies. *)

val add_used_in_current_handler : Name_occurrences.t -> t -> t
(** Add name occurrences used in the body of the current continuation's
    handler, *excluding* uses in apply_cont expressions, which are tracked
    separately. *)

val add_apply_result_cont : Continuation.t -> t -> t
(** Add the given continuation as being used as the return continuation for
    a function call. *)

val add_apply_cont_args : Continuation.t -> Name_occurrences.t list -> t -> t
(** Add, for the current continuation handler, uses for an apply cont of the
    given continuation with given arguments occurrences. *)

val add_extra_params_and_args :
  Continuation.t -> Continuation_extra_params_and_args.t -> t -> t
(** Add extra params and args to a continuation. *)


(* {2 Analysis} *)

type result = private {
  required_variables : Variable.Set.t;
  (** The set of all variables that are in fact used to compute the
      returned value of the function being analyzed. *)
}
(** The result of an analysis of the uses of variables in continuations. *)

val analyze
   : return_continuation:Continuation.t
  -> exn_continuation:Continuation.t
  -> t
  -> result
(** Analyze the uses. *)
