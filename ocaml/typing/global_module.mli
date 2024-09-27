[@@@ocaml.warning "+a-9-40-41-42"]

type ('name, 'value) duplicate =
  | Duplicate of { name : 'name; value1 : 'value; value2 : 'value }

module Name : sig
  type t = private {
    head : string;
    args : argument list;
  }
  and argument = {
    param : t;
    value : t;
  }

  include Identifiable.S with type t := t

  val create : string -> argument list -> (t, (t, t) duplicate) Result.t

  val create_exn : string -> argument list -> t

  val to_string : t -> string
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
  visible_args : (Name.t * t) list;
  hidden_args : (Name.t * t) list;
}

include Identifiable.S with type t := t

val create
   : string
  -> (Name.t * t) list
  -> hidden_args:(Name.t * t) list
  -> (t, (Name.t, t) duplicate) Result.t

val create_exn
   : string
  -> (Name.t * t) list
  -> hidden_args:(Name.t * t) list
  -> t

val to_string : t -> string

val to_name : t -> Name.t

val all_args : t -> (Name.t * t) list

(** A map from parameter names to their values. Hidden arguments aren't relevant
    in the parameter names, so they're represented by [Name.t]s here. *)
type subst = t Name.Map.t

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

(** Check that a substitution is a valid (possibly partial) instantiation of
    a module with the given parameter list. Each name being substituted must
    appear in the list. *)
val check : subst -> t list -> bool

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
