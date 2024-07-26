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

(** Zero_alloc attributes are checked
    in late stages of compilation in the backend.
    Registering them helps detect code that is not checked,
    because it is optimized away by the middle-end.  *)
val register_zero_alloc_attribute : string Location.loc -> unit
val mark_zero_alloc_attribute_checked : string -> Location.t -> unit

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
val warn_unchecked_zero_alloc_attribute : unit -> unit

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

module Attributes_filter : sig
  type t

  val create : (string list * bool) list -> t
end

(** [filter_attributes (Attributes_filter.create nms_and_conds) attrs] finds
    those attrs which appear in one of the sublists of nms_and_conds with
    cond=true.

    Each element [(nms, conds)] of the [nms_and_conds] list is a list of
    attribute names along with a boolean indicating whether to include
    attributes with those names in the output.  The boolean is used to
    accomodate different compiler configurations (e.g., we may want to check for
    "unrolled" only in the case where flambda or flambda2 is configured).  We
    handle this by taking a bool, rather than simply passing fewer nms in those
    cases, to support misplaced attribute warnings - the attribute should not
    count as misplaced if the compiler could use it in some configuration.
*)
val filter_attributes :
  ?mark:bool ->
  Attributes_filter.t -> Parsetree.attributes -> Parsetree.attributes

(** [find_attribute] behaves like [filter_attribute], except that it returns at
    most one matching attribute and issues a "duplicated attribute" warning if
    there are multiple matches. *)
val find_attribute :
  ?mark_used:bool -> Attributes_filter.t -> Parsetree.attributes ->
  Parsetree.attribute option

val warn_on_literal_pattern: Parsetree.attributes -> bool
val explicit_arity: Parsetree.attributes -> bool

val has_unboxed: Parsetree.attributes -> bool
val has_boxed: Parsetree.attributes -> bool

val parse_standard_interface_attributes : Parsetree.attribute -> unit
val parse_standard_implementation_attributes : Parsetree.attribute -> unit

val has_no_mutable_implied_modalities: Parsetree.attributes -> bool
val has_local_opt: Parsetree.attributes -> bool
val has_layout_poly: Parsetree.attributes -> bool
val has_curry: Parsetree.attributes -> bool
val has_or_null_reexport : Parsetree.attributes -> bool

(* Setting use:true will raise a warning if the attribute has an invalid parameter;
   tailcall should only be called with use:true once to generate exactly one alert. *)
(* CR less-tco: Maybe remove use:true *)
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
  in_signature:bool -> default_arity:int -> Parsetree.attributes ->
  zero_alloc_attribute

(* This returns the [zero_alloc_assume] if the input is an assume.  Otherwise,
   it returns None. If the input attribute is [Check], this issues a warning. *)
val zero_alloc_attribute_only_assume_allowed :
  zero_alloc_attribute -> zero_alloc_assume option

val assume_zero_alloc : zero_alloc_assume -> Zero_alloc_utils.Assume_info.t

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
