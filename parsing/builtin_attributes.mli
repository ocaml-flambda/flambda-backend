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

(** Support for the builtin attributes:

    - ocaml.afl_inst_ratio
    - ocaml.alert
    - ocaml.boxed
    - ocaml.deprecated
    - ocaml.deprecated_mutable
    - ocaml.explicit_arity
    - ocaml.flambda_o3
    - ocaml.flambda_oclassic
    - ocaml.immediate
    - ocaml.immediate64
    - ocaml.inline
    - ocaml.inlined
    - ocaml.noalloc
    - ocaml.poll
    - ocaml.ppwarning
    - ocaml.specialise
    - ocaml.specialised
    - ocaml.tailcall
    - ocaml.tail_mod_cons
    - ocaml.unboxed
    - ocaml.unsafe_allow_any_mode_crossing
    - ocaml.untagged
    - ocaml.unrolled
    - ocaml.warnerror
    - ocaml.warning
    - ocaml.warn_on_literal_pattern

    {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

(** {2 Attribute tracking for warning 53} *)

(** [register_attr] must be called on the locations of all attributes that
    should be tracked for the purpose of misplaced attribute warnings.  In
    particular, it should be called on all attributes that are present in the
    source program except those that are contained in the payload of another
    attribute (because these may be left behind by a ppx and intentionally
    ignored by the compiler).

    The [current_phase] argument indicates when this function is being called
    - either when an attribute is created in the parser or when we see an
    attribute while running the check in the [Ast_invariants] module.  This is
    used to ensure that we track only attributes from the final version of the
    parse tree: we skip adding attributes seen at parse time if we can see that
    a ppx will be run later, because the [Ast_invariants] check is always run on
    the result of a ppx.

    Note that the [Ast_invariants] check is also run on parse trees created from
    marshalled ast files if no ppx is being used, ensuring we don't miss
    attributes in that case.
*)
type current_phase = Parser | Invariant_check
val register_attr : current_phase -> string Location.loc -> unit

(** Marks the attributes hiding in the payload of another attribute used, for
    the purposes of misplaced attribute warnings (see comment on
    [current_phase] above).  In the parser, it's simplest to add these to
    the table and remove them later, rather than threading through state
    tracking whether we're in an attribute payload. *)
val mark_payload_attrs_used : Parsetree.payload -> unit

(** Issue misplaced attribute warnings for all attributes created with
    [mk_internal] but not yet marked used. Does nothing if compilation
    is stopped before lambda due to command-line flags. *)
val warn_unused : unit -> unit

(** {3 Warning 53 helpers for environment attributes}

    Some attributes, like deprecation markers, do not affect the compilation of
    the definition on which they appear, but rather result in warnings on future
    uses of that definition.  This is implemented by moving the raw attributes
    into the environment, where they will be noticed on future accesses.

    To make misplaced attribute warnings work appropriately for these
    attributes, we mark them "used" when they are moved into the environment.
    This is done with the helper functions in this section.
*)

(** Marks the attribute used for the purposes of misplaced attribute warnings if
    it is an alert.  Call this when moving things allowed to have alert
    attributes into the environment. *)
val mark_alert_used : Parsetree.attribute -> unit

(** The same as [List.iter mark_alert_used]. *)
val mark_alerts_used : Parsetree.attributes -> unit

(** Marks "warn_on_literal_pattern" attributes used for the purposes of
    misplaced attribute warnings.  Call this when moving constructors into the
    environment. *)
val mark_warn_on_literal_pattern_used : Parsetree.attributes -> unit

(** Marks "deprecated_mutable" attributes used for the purposes of misplaced
    attribute warnings.  Call this when moving labels of mutable fields into the
    environment. *)
val mark_deprecated_mutable_used : Parsetree.attributes -> unit

(** {3 Warning 53 helpers for zero alloc}

    Zero_alloc attributes are checked
    in late stages of compilation in the backend.
    Registering them helps detect code that is not checked,
    because it is optimized away by the middle-end.  *)
val register_zero_alloc_attribute : string Location.loc -> unit
val mark_zero_alloc_attribute_checked : string -> Location.t -> unit
val warn_unchecked_zero_alloc_attribute : unit -> unit

(** {2 Helpers for alert and warning attributes} *)

val check_alerts: Location.t -> Parsetree.attributes -> string -> unit
val check_alerts_inclusion:
  def:Location.t -> use:Location.t -> Location.t -> Parsetree.attributes ->
  Parsetree.attributes -> string -> unit

(** Find alerts (and mark them used, wrt misplaced attribute warnings) *)
val alerts_of_attrs: Parsetree.attributes -> Misc.alerts
val alerts_of_sig: mark:bool -> Parsetree.signature -> Misc.alerts
val alerts_of_str: mark:bool -> Parsetree.structure -> Misc.alerts

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

(** {2 Helpers for searching for particular attributes} *)

(** [has_attribute name attrs] is true if an attribute with name [name] or
    ["ocaml." ^ name] is present in [attrs].  It marks that attribute used for
    the purposes of misplaced attribute warnings. *)
val has_attribute : string -> Parsetree.attributes -> bool

(** [select_attributes actions attrs] finds the elements of [attrs] that appear
    in [actions] and either returns them or just marks them used, according to
    the corresponding [attr_action].

    Each element [(nm, action)] of the [actions] list is an attribute along with
    an [attr_action] specifying what to do with that attribute.  The action is
    used to accommodate different compiler configurations.  If an attribute is
    used only in some compiler configurations, it's important that we still look
    for it and mark it used when compiling with other configurations.
    Otherwise, we would issue spurious misplaced attribute warnings. *)
type attr_action = Mark_used_only | Return
val select_attributes :
  (string * attr_action) list -> Parsetree.attributes -> Parsetree.attributes

(** [attr_equals_builtin attr s] is true if the name of the attribute is [s] or
    ["ocaml." ^ s].  This is useful for manually inspecting attribute names, but
    note that doing so will not result in marking the attribute used for the
    purpose of warning 53, so it is usually preferrable to use [has_attribute]
    or [select_attributes]. *)
val attr_equals_builtin : Parsetree.attribute -> string -> bool

val warn_on_literal_pattern: Parsetree.attributes -> bool
val explicit_arity: Parsetree.attributes -> bool

val has_unboxed: Parsetree.attributes -> bool
val has_boxed: Parsetree.attributes -> bool

val has_unsafe_allow_any_mode_crossing : Parsetree.attributes -> bool

val parse_standard_interface_attributes : Parsetree.attribute -> unit
val parse_standard_implementation_attributes : Parsetree.attribute -> unit

(** The attribute placed on the inner [Ptyp_arrow] node in [x -> (y -> z)]
    (meaning the [y -> z] node) to indicate parenthesization. This is relevant
    for locals, as [local_ x -> (y -> z)] is different than
    [local_ x -> y -> z].
*)
val curry_attr_name : string
val curry_attr : Location.t -> Parsetree.attribute

val has_no_mutable_implied_modalities: Parsetree.attributes -> bool
val has_local_opt: Parsetree.attributes -> bool
val has_layout_poly: Parsetree.attributes -> bool
val has_curry: Parsetree.attributes -> bool
val has_or_null_reexport : Parsetree.attributes -> bool

val tailcall : Parsetree.attributes ->
    ([`Tail|`Nontail|`Tail_if_possible] option, [`Conflict]) result

(* CR layouts v1.5: Remove everything except for [Immediate64] and [Immediate]
   after rerouting [@@immediate]. *)
type jkind_attribute =
  | Immediate64
  | Immediate

val jkind_attribute_to_string : jkind_attribute -> string
val jkind_attribute_of_string : string -> jkind_attribute option

(* [jkind] gets the first jkind in the attributes if one is present.  All such
   attributes can be provided even in the absence of the layouts extension
   as the attribute mechanism predates layouts.
*)
val jkind : Parsetree.attributes -> jkind_attribute Location.loc option

(** Finds the first "error_message" attribute, marks it as used, and returns its
    string payload. Returns [None] if no such attribute is present.

    There should be at most one "error_message" attribute, additional ones are sliently
    ignored. **)
val error_message_attr : Parsetree.attributes -> string option

(** [get_int_payload] is a helper for working with attribute payloads.
    Given a payload that consist of a structure containing exactly
    {[
      PStr [
        {pstr_desc =
           Pstr_eval (Pexp_constant (Pconst_integer(i, None)), [])
        }
      ]
    ]}
    it returns [i].
  *)
val get_int_payload : Parsetree.payload -> (int, unit) Result.t

(** [get_optional_bool_payload] is a helper for working with attribute payloads.
    It behaves like [get_int_payload], except that it looks for a boolean
    constant rather than an int constant, and returns [None] rather than [Error]
    if the payload is empty. *)
val get_optional_bool_payload :
    Parsetree.payload -> (bool option, unit) Result.t

(** [parse_id_payload] is a helper for parsing information from an attribute
   whose payload is an identifier. If the given payload consists of a single
   identifier, that identifier is looked up in the association list.  The result
   is returned, if it exists.  The [empty] value is returned if the payload is
   empty.  Otherwise, [Error ()] is returned and a warning is issued. *)
val parse_optional_id_payload :
  string -> Location.t -> empty:'a -> (string * 'a) list ->
  Parsetree.payload -> ('a,unit) Result.t

(* Support for zero_alloc *)
type zero_alloc_check =
  { strict: bool;
    (* [strict=true] property holds on all paths.
       [strict=false] if the function returns normally,
       then the property holds (but property violations on
       exceptional returns or diverging loops are ignored).
       This definition may not be applicable to new properties. *)
    opt: bool;
    arity: int;
    loc: Location.t;
    custom_error_msg : string option;
  }

type zero_alloc_assume =
  { strict: bool;
    never_returns_normally: bool;
    never_raises: bool;
    (* [never_raises=true] the function never returns
       via an exception. The function (directly or transitively)
       may raise exceptions that do not escape, i.e.,
       handled before the function returns. *)
    arity: int;
    loc: Location.t;
  }

type zero_alloc_attribute =
  | Default_zero_alloc
  | Ignore_assert_all
  | Check of zero_alloc_check
  | Assume of zero_alloc_assume

val is_zero_alloc_check_enabled : opt:bool -> bool

(* Gets a zero_alloc attribute.  [~in_signature] controls both whether the
   "arity n" field is allowed, and whether we track this attribute for
   warning 199. *)
val get_zero_alloc_attribute :
  in_signature:bool -> on_application:bool-> default_arity:int -> Parsetree.attributes ->
  zero_alloc_attribute

(* This returns the [zero_alloc_assume] if the input is an assume.  Otherwise,
   it returns None. If the input attribute is [Check], this issues a warning. *)
val zero_alloc_attribute_only_assume_allowed :
  zero_alloc_attribute -> zero_alloc_assume option

(* [inferred] is true only for "assume" annotations added by the compiler onto
   applications based on the type of the callee, to distinguish them from annotations
   added by the user. Inferred annotations are removed if the callee is inlined, to ensure
   that inlining and other middle-end optimizations preserve the relaxed meaning of
   zero_alloc.  Note that "inferred" assume annotations are different assume annotations
   "propagated" by the compiler from function definition to all primitives in the
   function's body (via scopes / debug info). *)
val assume_zero_alloc : inferred:bool -> zero_alloc_assume -> Zero_alloc_utils.Assume_info.t

type tracing_probe =
  { name : string;
    name_loc : Location.t;
    enabled_at_init : bool;
    arg : Parsetree.expression;
  }

(* Gets the payload of a [probe] extension node. Example syntax of a probe
   that's disabled by default:

   [%probe "my_probe" arg]

   You can use [enabled_at_init] to control whether the probe is enabled
   by default:

   [%probe "my_probe" ~enabled_at_init:true arg]
*)
val get_tracing_probe_payload :
  Parsetree.payload -> (tracing_probe, unit) result
