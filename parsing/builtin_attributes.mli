(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Support for some of the builtin attributes

    - ocaml.deprecated
    - ocaml.alert
    - ocaml.error
    - ocaml.ppwarning
    - ocaml.warning
    - ocaml.warnerror
    - ocaml.explicit_arity (for camlp4/camlp5)
    - ocaml.warn_on_literal_pattern
    - ocaml.deprecated_mutable
    - ocaml.boxed / ocaml.unboxed
    - ocaml.nolabels
    - ocaml.inline
    - ocaml.afl_inst_ratio
    - ocaml.flambda_o3
    - ocaml.flambda_oclassic
    - jkind attributes:
      - ocaml.any
      - ocaml.value
      - ocaml.void
      - ocaml.immediate
      - ocaml.immediate64

    {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)


(** [register_attr] must be called on the locations of all attributes that
    should be tracked for the purpose of misplaced attribute warnings.  In
    particular, it should be called on all attributes that are present in the
    source program except those that are contained in the payload of another
    attribute (because these may be left behind by a ppx and intentionally
    ignored by the compiler).

    The [attr_tracking_time] argument indicates when the attr is being added for
    tracking - either when it is created in the parser or when we see it while
    running the check in the [Ast_invariants] module.  This ensures that we
    track only attributes from the final version of the parse tree: we skip
    adding attributes at parse time if we can see that a ppx will be run later,
    because the [Ast_invariants] check is always run on the result of a ppx.

    Note that the [Ast_invariants] check is also run on parse trees created from
    marshalled ast files if no ppx is being used, ensuring we don't miss
    attributes in that case.
*)
type attr_tracking_time = Parser | Invariant_check
val register_attr : attr_tracking_time -> string Location.loc -> unit

(** Marks alert attributes used for the purposes of misplaced attribute
    warnings.  Call this when moving things with alert attributes into the
    environment. *)
val mark_alert_used : Parsetree.attribute -> unit
val mark_alerts_used : Parsetree.attributes -> unit

(** Properties such as the zero_alloc attribute that are checked
    in late stages of compilation in the backend.
    Registering them helps detect code that is not checked,
    because it is optimized away by the middle-end.  *)
val register_property : string Location.loc -> unit
val mark_property_checked : string -> Location.t -> unit

(** Marks "warn_on_literal_pattern" attributes used for the purposes of
    misplaced attribute warnings.  Call this when moving things with alert
    attributes into the environment. *)
val mark_warn_on_literal_pattern_used : Parsetree.attributes -> unit

(** Marks the attributes hiding in the payload of another attribute used, for
    the purposes of misplaced attribute warnings (see comment on
    [attr_tracking_time] above).  In the parser, it's simplest to add these to
    the table and remove them later, rather than threading through state
    tracking whether we're in an attribute payload. *)
val mark_payload_attrs_used : Parsetree.payload -> unit

(** Issue misplaced attribute warnings for all attributes created with
    [mk_internal] but not yet marked used. *)
val warn_unused : unit -> unit

val check_alerts: Location.t -> Parsetree.attributes -> string -> unit
val check_alerts_inclusion:
  def:Location.t -> use:Location.t -> Location.t -> Parsetree.attributes ->
  Parsetree.attributes -> string -> unit

(** Find alerts (and mark them used, wrt misplaced attribute warnings) *)
val alerts_of_attrs: Parsetree.attributes -> Misc.alerts
val alerts_of_sig: Parsetree.signature -> Misc.alerts
val alerts_of_str: Parsetree.structure -> Misc.alerts

val check_deprecated_mutable:
    Location.t -> Parsetree.attributes -> string -> unit
val check_deprecated_mutable_inclusion:
  def:Location.t -> use:Location.t -> Location.t -> Parsetree.attributes ->
  Parsetree.attributes -> string -> unit

val error_of_extension: Parsetree.extension -> Location.error

val warning_attribute: ?ppwarning:bool -> Parsetree.attribute -> unit
  (** Apply warning settings from the specified attribute.
      "ocaml.warning"/"ocaml.warnerror" (and variants without the prefix) are
      processed and marked used for warning 53.  Other attributes are ignored.

      Also implement ocaml.ppwarning (unless ~ppwarning:false is
      passed).
  *)

val warning_scope:
  ?ppwarning:bool ->
  Parsetree.attributes -> (unit -> 'a) -> 'a
  (** Execute a function in a new scope for warning settings.  This
      means that the effect of any call to [warning_attribute] during
      the execution of this function will be discarded after
      execution.

      The function also takes a list of attributes which are processed
      with [warning_attribute] in the fresh scope before the function
      is executed.
  *)

(** [has_attribute names attrs] is true if an attribute named in [names] is
    present in [attrs].  It marks that attribute used for the purposes of
    misplaced attribute warnings. *)
val has_attribute : string list -> Parsetree.attributes -> bool

(** [filter_attributes nms_and_conds attrs] finds those attrs which
    appear in one of the sublists of nms_and_conds with cond=true.

    Each element [(nms, conds)] of the [nms_and_conds] list is a list of
    attribute names along with a boolean indicating whether to include
    attributes with those names in the output.  The boolean is used to
    accomodate different compiler configurations (e.g., we may want to check for
    "unrolled" only in the case where flambda or flambda2 is configured).  We
    handle this by taking a bool, rather than simply passing fewer nms in those
    cases, to support misplaced attribute warnings - the attribute should not
    count as misplaced if the compiler could use it in some configuration. *)
val filter_attributes :
  (string list * bool) list -> Parsetree.attributes -> Parsetree.attributes

val warn_on_literal_pattern: Parsetree.attributes -> bool
val explicit_arity: Parsetree.attributes -> bool

val has_unboxed: Parsetree.attributes -> bool
val has_boxed: Parsetree.attributes -> bool

val parse_standard_interface_attributes : Parsetree.attribute -> unit
val parse_standard_implementation_attributes : Parsetree.attribute -> unit

val has_local_opt: Parsetree.attributes -> bool
val has_curry: Parsetree.attributes -> bool

(* These functions report Error if the builtin extension.* attributes
   are present despite the extension being disabled *)
val has_local: Parsetree.attributes -> (bool,unit) result
val has_global: Parsetree.attributes -> (bool,unit) result
val tailcall : Parsetree.attributes ->
    ([`Tail|`Nontail|`Tail_if_possible] option, [`Conflict]) result

val has_unique: Parsetree.attributes -> (bool,unit) result

val has_once : Parsetree.attributes -> (bool, unit) result

(* [jkind] gets the jkind in the attributes if one is present.  We always
   allow the [value] annotation, even if the layouts extensions are disabled.
   If [~legacy_immediate] is true, we allow [immediate] and [immediate64]
   attributes even if the layouts extensions are disabled - this is used to
   support the original immediacy attributes, which are now implemented in terms
   of jkinds.

   The return value is [Error <jkind>] if a jkind attribute is present but
   not allowed by the current set of extensions.  Otherwise it is [Ok None] if
   there is no jkind annotation and [Ok (Some jkind)] if there is one.

   - If no layout extensions are on and [~legacy_immediate] is false, this will
     always return [Ok None], [Ok (Some Value)], or [Error ...].
   - If no layout extensions are on and [~legacy_immediate] is true, this will
     error on [void], [float64], or [any], but allow [immediate], [immediate64],
     and [value].
   - If the [Layouts_beta] extension is on, this behaves like the previous case
     regardless of the value of [~legacy_immediate], except that it allows
     [float64] and [any].
   - If the [Layouts_alpha] extension is on, this can return any jkind and
     never errors.

   Currently, the [Layouts] extension is ignored - it's no different than
   turning on no layout extensions.

   This is not the only place the layouts extension level is checked.  If you're
   changing what's allowed in a given level, you may also need to make changes
   in the parser, Jkind.get_required_layouts_level, and Typeopt.
*)
(* CR layouts: we should eventually be able to delete ~legacy_immediate (after we
   turn on layouts by default). *)
val jkind : legacy_immediate:bool -> Parsetree.attributes ->
  (Jane_asttypes.jkind_annotation option,
   Jane_asttypes.jkind_annotation) result
