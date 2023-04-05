[@@@ocaml.warning "+a-9-40-41-42"]

module Name : sig
  type t = {
    head : string;
    args : (t * t) list;
  }

  include Identifiable.S with type t := t
end

(** A name, with both the arguments it's being passed and the parameters it's
    still waiting for. Note that each remaining parameter has _two_ names
    associated with it. The first is the parameter from the perspective of the
    original module (i.e., the head of [name]), and the second is the
    perspective of someone passing in additional parameters. Normally these
    are the same: if [M] takes parameters [X] and [Y], neither of which is
    itself parameterised, we might have (abbreviating nested records):

    {v
      { head: M; args: [ X, Foo ]; params: [ Y, Y ] }
    v}

    This says we've so far passed [M] the parameter [Foo] as [X] and it still
    awaits a parameter [Y].

    However, suppose instead [Y] is parameterised by [X]. Then [M] still takes
    two parameters [X] and [Y], but now once [X] is set to [Foo], [Y] needs to
    become [Y[X\Foo]] and we have:

    {v
      { head: M; args: [ X, Foo ]; params: [ Y, Y[X\Foo] ] }
    v}

    This reflects the fact that further specialisation requires passing an
    argument for [Y[X\Foo]] rather than [Y]. (Here, [Y[X\Foo]] stands for the
    record [{ head = Y; args = [ X, Foo ] }] of type [t].)
*)
type t = {
  head : string;
  args : (Name.t * t) list;
  params : (Name.t * t) list;
}

include Identifiable.S with type t := t

val to_name : t -> Name.t

type subst = t Name.Map.t

val subst : t -> subst -> t

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
