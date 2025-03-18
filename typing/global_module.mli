module Parameter_name : sig
  type t

  val of_string : string -> t

  val to_string : t -> string

  include Identifiable.S with type t := t
end

type 'value duplicate =
  | Duplicate of { name : Parameter_name.t; value1 : 'value; value2 : 'value }

module Argument : sig
  type 'value t = {
    param : Parameter_name.t;
    value : 'value;
  }
end

module Name : sig
  type t = private {
    head : string;
    args : argument list;
  }
  and argument = t Argument.t

  include Identifiable.S with type t := t

  val create : string -> argument list -> (t, t duplicate) Result.t

  val create_exn : string -> argument list -> t

  val create_no_args : string -> t

  val of_parameter_name : Parameter_name.t -> t

  val to_string : t -> string

  (* Find this name in a map keyed by parameter names. Preferrable to converting
     this to a [Parameter_name.t] since it may not in fact be a parameter name *)
  val find_in_parameter_map : t -> 'a Parameter_name.Map.t -> 'a option

  val mem_parameter_set : t -> Parameter_name.Set.t -> bool
end

(** An elaborated form of name in which all arguments are expressed, including
    those being passed implicitly from one module to another by the subset rule
    for parameterised modules. Normally, these "hidden" arguments simply say to
    pass [X] as [X] for some module [X], but if there are parameterised
    parameters, the hidden arguments can get more complex.

    Suppose [M] takes parameters [X] and [Y], neither of which is itself
    parameterised. If someone is passing [Foo] as the value of [X], then, we
    will have (abbreviating nested records):

    {[
      { head: M; visible_args: [ X, Foo ]; hidden_args: [ Y, Y ] }
    ]}

    This represents that [X] is explicitly being given the value [Foo] and [Y]
    (the parameter) is implicitly getting the value [Y] (the argument currently
    in scope).

    However, suppose instead [Y] is parameterised by [X]. Then [M] still takes
    two parameters [X] and [Y], but now once [X] has the value [Foo], [Y]
    requires _that particular_ [X]:

    {[
      { head: M; visible_args: [ X, Foo ]; hidden_args: [ Y, Y[X:Foo] ] }
    ]}

    Importantly, the _parameters_ [X] and [Y] never change: they are names that
    appear in [m.ml] and [m.cmi]. But further specialisation requires passing
    specifically a [Y[X:Foo]] rather than a [Y]. (Here, [Y[X:Foo]] stands for
    the record [{ head = Y; visible_args = [ X, Foo ]; hidden_args = [] }] of
    type [t].)
*)
type t = private {
  head : string;
  visible_args : argument list;
  hidden_args : argument list;
}
and argument = t Argument.t

include Identifiable.S with type t := t

val create
   : string -> argument list -> hidden_args:Parameter_name.t list
  -> (t, t duplicate) Result.t

val create_exn : string -> argument list -> hidden_args:Parameter_name.t list -> t

val to_string : t -> string

val to_name : t -> Name.t

(** A map from parameter names to their values. *)
type subst = t Parameter_name.Map.t

(** Apply a substitution to the given global. If it appears in the substitution
    directly (that is, its [Name.t] form is a key in the map), this simply
    performs a lookup. Otherwise, we perform a _revealing substitution_: if the
    value of a hidden argument is a key in the substitution, the argument becomes
    visible. Otherwise, substitution recurses into arguments (both hidden and
    visible) as usual. See [global_test.ml] for examples. *)
val subst : t -> subst -> t * [ `Changed | `Did_not_change ]

(** Apply a substitution to the arguments and parameters in [t] but not to [t]
    itself. Useful if [subst] is constructed from some parameter-argument pairs
    and [t] is one of the parameters, since we want to handle any
    interdependencies but the substitution applied to [t] itself is
    uninterestingly just the corresponding value. *)
val subst_inside : t -> subst -> t

val find_in_parameter_map : t -> 'a Parameter_name.Map.t -> 'a option

(** Check that a substitution is a valid (possibly partial) instantiation of
    a module with the given parameter list. Each name being substituted must
    appear in the list. *)
val check : subst -> Parameter_name.t list -> bool

(** Returns [true] if [hidden_args] is empty and all argument values (if any)
    are also complete. This is a stronger condition than full application, and
    (unless the whole global is itself a parameter) it's equivalent to the
    global being a static constant, since any parameters being used would have
    to show up in a [hidden_args] somewhere. (Importantly, it's not possible
    that a parameter is being used as an argument to a different parameter,
    since a module can be declared to be an argument for up to one parameter.)

    CR lmaurer: Make sure we're checking for the user redundantly passing an
    parameter as an argument. This should be accepted and ignored, lest we
    count the parameter as filled and consider something completely
    instantiated. *)
val is_complete : t -> bool

(** Returns [true] if this name has at least one argument (either hidden or
    visible). *)
val has_arguments : t -> bool

module Precision : sig
  (** Whether a global's elaborated form is known exactly. For example, given
      the elaborated form [Foo{Bar; Baz}], if we never loaded foo.cmi then
      we don't actually know whether [Foo] takes [Bar] or [Baz]. *)
  type t =
    | Exact (** The base module takes exactly the arguments being passed. *)
    | Approximate
        (** The base module takes some subset of the arguments being passed
            (possibly all of them). *)

  val print : Format.formatter -> t -> unit

  val output : out_channel -> t -> unit
end

module With_precision : sig
  type nonrec t = t * Precision.t

  val equal : t -> t -> bool

  exception Inconsistent

  (** Given two elaborated forms of the same name and their precision, reconcile
      them. In any case, if the visible parts of the globals disagree, raise
      [Inconsistent] (because they don't in fact elaborate the same [Name.t]).
      For the hidden parts, we treat an exact [t] as requiring equality and an
      approximate [t] as specifying an upper bound. Thus exact vs. exact checks
      for equality, exact vs. approximate checks the upper bound, and
      approximate vs. approximate takes the least upper bound (that is, the
      intersection). *)
  val meet : t -> t -> t

  val print : Format.formatter -> t -> unit

  val output : out_channel -> t -> unit
end
