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

    This module aims mainly at tracking uses of variables (other things may be
    added later on), with the aim of:

    - removing unused parameters of *recursive* continuations;

    - moving allocations out of the hot path of recursive continuations (e.g.
    the allocation of a float that was unboxed by the simplifier). *)

(** Type alias for convenience. *)
type t = Flow_types.Acc.t

(** Printing *)
val print : Format.formatter -> t -> unit

(** "Consume" the extra args of an accumulator in order to add them to the
    regular args and parameters in the continuation info of each continuation. *)
val extend_args_with_extra_args : t -> t

(** A name for the incorrec tdummy toplevel cont used to initialize the acc. *)
val wrong_dummy_toplevel_cont_name : string

(* {2 Creation and updates} *)

(** Empty uses *)
val empty : unit -> t

(** Initialize the analysis so that the stack consists of a single toplevel
    continuation. *)
val init_toplevel :
  dummy_toplevel_cont:Continuation.t -> Bound_parameters.t -> t -> t

(** Add a new continuation on the stack. Used when entering a continuation
    handler. *)
val enter_continuation :
  Continuation.t ->
  recursive:bool ->
  is_exn_handler:bool ->
  Bound_parameters.t ->
  t ->
  t

(** Pop the current top of the stack. Used when exiting the current continuation
    handler. *)
val exit_continuation : Continuation.t -> t -> t

(** That variable is defined in the current handler *)
val record_defined_var : Variable.t -> t -> t

(** Add a variable binding from the current handler. *)
val record_var_binding :
  Variable.t -> Name_occurrences.t -> generate_phantom_lets:bool -> t -> t

(** Record a let-binding *)
val record_let_binding :
  rewrite_id:Named_rewrite_id.t ->
  generate_phantom_lets:bool ->
  let_bound:Bound_pattern.t ->
  simplified_defining_expr:Simplified_named.t ->
  t ->
  t

(** Add a variable binding to the symbol. Projections might get recorded
    multiple times. *)
val record_symbol_projection : Variable.t -> Name_occurrences.t -> t -> t

(** Add a symbol binding from the current handler. *)
val record_symbol_binding : Symbol.t -> Name_occurrences.t -> t -> t

(** Add a code id binding from the current handler. *)
val record_code_id_binding : Code_id.t -> Name_occurrences.t -> t -> t

(** Add a value slot from the current handler. *)
val record_value_slot : Name.t -> Value_slot.t -> Name_occurrences.t -> t -> t

(** Add name occurrences used in the body of the current continuation's handler,
    *excluding* uses in apply_cont expressions, which are tracked separately. *)
val add_used_in_current_handler : Name_occurrences.t -> t -> t

(** Add the given continuation as being used as the return continuation for a
    function call. *)
val add_apply_conts :
  result_cont:(Apply_cont_rewrite_id.t * Continuation.t) option ->
  exn_cont:Apply_cont_rewrite_id.t * Exn_continuation.t ->
  result_arity:[`Unarized] Flambda_arity.t ->
  t ->
  t

(** Add, for the current continuation handler, uses for an apply cont of the
    given continuation with given arguments occurrences. *)
val add_apply_cont_args :
  rewrite_id:Apply_cont_rewrite_id.t ->
  Continuation.t ->
  Simple.t list ->
  t ->
  t

(** Add extra params and args to a continuation. *)
val add_extra_params_and_args :
  Continuation.t -> Continuation_extra_params_and_args.t -> t -> t
