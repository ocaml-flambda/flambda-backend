(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2022 OCamlPro SAS                                    *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {1 Translation environment} *)

(** Environment for Flambda to Cmm translation *)
type t

(** Create an environment for translating a toplevel expression. *)
val create :
  Exported_offsets.t ->
  Exported_code.t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  t

(** Given an existing environment providing the "global" information (such as
    the exported code structure), create an environment for translating the body
    of a function. *)
val enter_function_body :
  t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  t

(** {2 Continuations} *)

(** Returns the return continuation of the environment. *)
val return_continuation : t -> Continuation.t

(** Returns the exception continuation of the environment. *)
val exn_continuation : t -> Continuation.t

(** {2 Code and closures} *)

(** Retrieve the code metadata for a given code ID. This function will produce
    fatal error if the code ID was not in the [Exported_code.t] passed to
    [create]. *)
val get_code_metadata : t -> Code_id.t -> Code_metadata.t

(** Retrieve the closure offset information as passed to [create]. This can be
    used for things such as the compilation of function and value slot
    projections. *)
val exported_offsets : t -> Exported_offsets.t

(** {2 Variable bindings} *)

(** Extra information about bound variables, used for optimisation. *)
type extra_info =
  | Untag of Cmm.expression
      (** The variable is bound to the result of untagging the given Cmm
          expression. This allows to obtain the Cmm expression as it was before
          untagging. *)
  | Boxed_number  (** The variable is bound to a boxed number. *)

(** Create (and bind) a Cmm variable for the given Flambda variable, returning
    the new environment and the created variable. Will produce a fatal error if
    the given variable is already bound. *)
val create_variable : t -> Variable.t -> t * Backend_var.With_provenance.t

(** Same as {!create_variable} but for a list of variables. *)
val create_variables :
  t -> Variable.t list -> t * Backend_var.With_provenance.t list

(** Delayed let-bindings

    Let-bindings are delayed in a certain way to allow for potential reordering
    and inlining of the defining expressions of bound variables that are used
    exactly once. This behaviour (which are a more powerful version of the
    [Un_anf] pass from Flambda 1) does not change semantics. However they allow
    the Cmm code to adopt the forms expected by the Cmm optimisation functions
    (in [Cmm_helpers], for example the arithmetic optimisations) and the
    instruction selection pass (for example where matches may be done on
    function applications and their arguments). Without this, many pattern
    matches on the Cmm terms would fail, leading to poor code generation.

    The delaying process works by partitioning let-bindings into two categories
    according to the effects and coeffects of their defining expressions:

    - bindings whose defining expressions are _pure_, that is to say have
    neither effects nor coeffects;

    - bindings that have effects and/or coeffects.

    Bindings in the first category, by virtue of the absence of effects and
    coeffects, will be allowed to commute with any other bindings.

    The second category of bindings is organised into an ordered list of
    _stages_. A stage is a set of bindings that can all commute with each other.
    Bindings inside stages cannot commute across stages; and stages cannot
    commute.

    One stage consists of either:

    - an ordered list of bindings all of which have only coeffects; or

    - a single effectful binding.

    When a binding is considered for addition to the most recent stage, it may
    either be appended to that stage, or else the stage is "archived" and a new
    stage started containing the new binding.

    The defining expression of a binding used exactly once may be inlined only
    if the binding occurs in the current stage or is pure. Inlining of a binding
    causes it to be removed from its stage (empty stages are then removed from
    the head of the list of stages).

    Other bindings are delayed until they are explicitly flushed. Exactly which
    bindings get flushed at different points, for example prior to function
    calls or branching control flow, depends on decisions outside of this module
    (e.g. in [To_cmm_expr]). *)

(** Bind a variable to the given Cmm expression, to allow for delaying the
    let-binding. *)
val bind_variable :
  ?extra:extra_info ->
  t ->
  Variable.t ->
  num_normal_occurrences_of_bound_vars:
    Num_occurrences.t Variable.Map.t Or_unknown.t ->
  effects_and_coeffects_of_defining_expr:Effects_and_coeffects.t ->
  defining_expr:Cmm.expression ->
  t

(** Try and inline an Flambda variable using the delayed let-bindings. *)
val inline_variable :
  t -> Variable.t -> Cmm.expression * t * Effects_and_coeffects.t

(** Wrap the given Cmm expression with all the delayed let bindings accumulated
    in the environment. *)
val flush_delayed_lets :
  ?entering_loop:bool -> t -> (Cmm.expression -> Cmm.expression) * t

(** Fetch the extra info for a Flambda variable (if any). *)
val extra_info : t -> Variable.t -> extra_info option

(** {2 Continuation bindings} *)

(** Translation information for continuations. A continuation may either be
    translated as a static jump to a Cmm continuation (represented as a Cmm
    label), or inlined at any unique use site. *)
type cont = private
  | Jump of
      { cont : Cmm.label;
        param_types : Cmm.machtype list
      }
  | Inline of
      { handler_params : Bound_parameters.t;
        handler_params_occurrences : Num_occurrences.t Variable.Map.t;
        handler_body : Flambda.Expr.t
      }

(** Record that the given continuation should be compiled to a jump, creating a
    fresh Cmm continuation identifier for it. *)
val add_jump_cont :
  t -> Continuation.t -> param_types:Cmm.machtype list -> Cmm.label * t

(** Record that the given continuation should be inlined. *)
val add_inline_cont :
  t ->
  Continuation.t ->
  handler_params:Bound_parameters.t ->
  handler_params_occurrences:Num_occurrences.t Variable.Map.t ->
  handler_body:Flambda.Expr.t ->
  t

(** Register the given continuation as an exception handler and set up the extra
    Cmm mutable variables needed for any extra arguments. *)
val add_exn_handler :
  t ->
  Continuation.t ->
  Flambda_arity.t ->
  t * (Backend_var.t * Flambda_kind.t) list

(** Return whether the given continuation has been registered as an exception
    handler. *)
val is_exn_handler : t -> Continuation.t -> bool

(** Return the Cmm mutable variables associated with the given exception
    handler. *)
val get_exn_extra_args : t -> Continuation.t -> Backend_var.t list

(** Return the binding for a given continuation, describing whether it is to be
    compiled as a jump or inlined, etc. Produces a fatal error if given an
    unbound continuation. *)
val get_continuation : t -> Continuation.t -> cont

(** Returns the Cmm continuation identifier bound to a continuation. Produces a
    fatal error if given an unbound continuation, or a continuation that was
    registered (using [add_inline_cont]) to be inlined. *)
val get_cmm_continuation : t -> Continuation.t -> Cmm.label
