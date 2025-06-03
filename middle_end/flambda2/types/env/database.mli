(** {1 Database}

  This module implements a database of relational {e facts} we know about
  variables.

  The database is able to represent relations such as [Pisint = Is_int(x)] and
  [Pgettag = Get_tag(x)], and to accurately propagate them as we learn new
  equalities.

  Facilities are provided to:

    - Record a relation [x = R(y)] ;
    - Attach an extension that becomes available when the value of a variable
      [x] becomes known ;
    - Attach an extension that becomes available when the value of a relation
      [R(x)] becomes known ;
    - Find the value associated with [R(y)], if it exists ;
    - Find all the [y] such that [R(y) = x] is in the database, for some [x] ;
    - Find all the extensions that will become available when the value of a
      variable [x] becomes known.

  At the moment, only unary relations are supported.

  Note that relations are partial. The database considers that a relation
  [R(y)] holds as soon as an entry [R(y) = x] exists, but it is possible to
  attach extensions to relations that do not (yet) hold. For instance, it is
  possible to attach an extension to [Get_tag(y)] even if it is not known that
  [y] is a block.

  This module also provides an incremental/differential interface (levels)
  to be able to efficiently inspect the changes between two states of the
  database. *)

type t

include Contains_names.S with type t := t

val print : Format.formatter -> t -> unit

(** The empty database, containing no facts. *)
val empty : t

val is_empty : t -> bool

(** {2 Relations} *)

module Function : sig
  type t

  type descr =
    | Is_null
    | Is_int
    | Get_tag
    | Untag_imm
    | Tag_imm

  val descr : t -> descr

  val is_null : t

  val is_int : t

  val get_tag : t

  val untag_imm : t

  val tag_imm : t

  include Container_types.S_plus_iterator with type t := t

  val of_const :
    t -> Reg_width_const.t -> Reg_width_const.t Or_unknown_or_bottom.t
end

(** Record a new property [fn(arg) = result] in the database.

    This may instead create new aliases if there was already an entry for
    [fn(arg)] with a different value.

    {b Warning}: Both [arg] and [result] {b must be canonical} in the [aliases]
    structure. *)
val add_property :
  binding_time_resolver:(Name.t -> Binding_time.With_name_mode.t) ->
  binding_times_and_modes:('a * Binding_time.With_name_mode.t) Name.Map.t ->
  Aliases.t ->
  t ->
  Function.t ->
  arg:Simple.t ->
  result:Simple.t ->
  (t * Aliases.t) Or_bottom.t

(** Get the value associated with a property [fn(arg)].

    If [find_property] returns [Ok simple], then the property definitely exists
    and is equal to [simple] (for instance, if [find_property] returns [Ok] for
    the [Get_tag] property, the argument is definitely a block).

    If [find_property] returns [Bottom], then the property definitely does not
    exist (for instance, [find_property] might return [Bottom] for the [Get_tag]
    property on an argument that is known to be a tagged integer).

    If [find_property] returns [Unknown], it is not known whether the property
    exists (and what is value is) or does not exist.

    {b Warning}: If a [simple] is returned, it is {b NOT} guaranteed to be
    canonical. *)
val find_property :
  t -> Simple.t -> Function.t -> Simple.t Or_unknown_or_bottom.t

(** {2 Canonicalization}

    The database is intended to be kept canonical with respect to an [Aliases.t]
    structure (and uses an [Aliases.t] structure to record learnt aliases).

    Canonicalization ensures that properties that were known of a demoted name
    apply to its new canonical instead.

    The canonicalization process is {b delayed} and allows processing a batch of
    demotions at once through the [rebuild] function. *)

(** [rebuild aliases db demotions] applies the demotions in [demotions] to the
    database [db], normalizing the database.

    When new demotions occurs in the [aliases] structure, the database
    should be notified through [rebuild].

    Note that [rebuild] can itself introduce new aliases; for instance, if the
    database has entries [P(x) = a] and [P(y) = b], then rebuilding after the
    demotions [x -> y] will cause a new alias between [a] and [b] to be added by
    [rebuild]. Such aliases introduced by rebuild are {b not} recursively
    processed by [rebuild] and need to be handled by the caller.

    {b Note}: Demotions are as computed by the [Aliases.diff] function, i.e.
    an entry [name -> (simple, coercion)] means that [name] was demoted to
    [coercion^-1(simple)] (or [coercion(name)] was demoted to [simple]). *)
val rebuild :
  binding_time_resolver:(Name.t -> Binding_time.With_name_mode.t) ->
  binding_times_and_modes:('b * Binding_time.With_name_mode.t) Name.Map.t ->
  Aliases.t ->
  t ->
  (Simple.t * Coercion.t) Name.Map.t ->
  (t * Aliases.t) Or_bottom.t

(** [shortcut_aliases ~canonicalise db] applies the canonicalisation function
    [canonicalise] to the database [db].

    Assumes that the database has been properly normalized through calls to
    [rebuild] first.

    {b Warning}: This function loses sharing. *)
val shortcut_aliases : canonicalise:(Simple.t -> Simple.t) -> t -> t

(** {2 Switches}

    Switches are the mechanism by which we deal with (local) disjunction in the
    database.

    A switch can be recorded on a name or property. For each possible value of
    the name or property, a set of extension identifiers are provided. When the
    value for that name or property becomes known, the corresponding extension
    identifiers are activated.

    It is possible to peek at the state of the switches using the
    [switch_on_scrutinee] function, which will return the current switch
    associated with a name, encoding all the extensions that would become
    active if the value of that name was known.

    {b Note}: Switches can also function as a domain recording the possible
    values of a name or property (by providing an empty set of extensions for
    each possible value). *)

module Extension_id : sig
  include Container_types.S

  val create : unit -> t
end

val active_extensions : t -> Extension_id.Set.t

val add_switch_on_canonical :
  Simple.t ->
  ?default:Extension_id.Set.t ->
  arms:Extension_id.Set.t Reg_width_const.Map.t ->
  t ->
  t Or_bottom.t

val add_switch_on_property :
  Function.t ->
  Simple.t ->
  ?default:Extension_id.Set.t ->
  arms:Extension_id.Set.t Reg_width_const.Map.t ->
  t ->
  aliases:Aliases.t ->
  t Or_bottom.t

(** [switch_on_scrutinee db ~scrutinee] returns a pair [(known, other)] of
    extensions associated with the switch.

    If the value of the switch is in [known], the corresponding extension will
    be activated.

    If [other] is [Bottom], the value of the [scrutinee] is known to be one of
    the constants in [known]. If [other] is not [Bottom], the extensions in
    [other] will be activated if the value of [scrutinee] is not one of the
    constants in [known]. *)
val switch_on_scrutinee :
  t ->
  scrutinee:Simple.t ->
  Extension_id.Set.t Reg_width_const.Map.t * Extension_id.Set.t Or_bottom.t

(** {2 Extensions} *)

type extension

module Extension : sig
  type t = extension

  val fold :
    (Function.t -> arg:Name.t -> result:Simple.t -> 'a -> 'a) -> t -> 'a -> 'a

  val print : Format.formatter -> t -> unit

  val empty : t

  val is_empty : t -> bool

  val add_property : t -> Function.t -> arg:Name.t -> result:Simple.t -> t

  val free_names : t -> Name_occurrences.t

  val apply_renaming : t -> Renaming.t -> t

  val project_variables_out : to_project:Variable.Set.t -> t -> t

  val disjoint_union : t -> t -> t
end

(** {2 Incremental interface}

    The incremental interface allows to compute the {b difference} between two
    databases as a [level].

    {b Note}: The incremental interface makes use of physical equality to ensure
    good performance when computing the difference with previous instance of the
    same database. In particular, computing the difference between [db'] and
    [db] where [db'] has been obtained by performing a sequence of database
    operations on [db] is only linear on the size of the difference, and not
    on the size of [db] and [db'] themselves.

    The semantics of the incremental interface are however independent of the
    physical representation of the database (but it will be less efficient when
    sharing is not available). *)

type level

(** [cut target ~cut_after:source] constructs a new level starting in [source]
    and ending in [target].

    The [target] database must have been obtained by applying a sequence of
    database operations on the [source] database. *)
val cut : t -> cut_after:t -> level

module Level : sig
  (** A level represents the difference between a {e source} database (the start of the level) and a {e target}
      database (the end of the level), where the target database has been obtained by performing a
      sequence of operations on the source database. The level records the
      effect of these operations.

      A level on its own {b has no semantics}: it can only be understood in the
      context of the transition from the start of the level to the end of the
      level. *)

  type t = level

  val print : Format.formatter -> t -> unit

  (** The empty level.

      This is a special level that contains no change, and can represent a
      transition from any database to itself. It is the only level that does not
      have well-defined source and target databases. *)
  val empty : t

  (** [is_empty t] returns [true] iff [t] is an empty level.

      An empty level is a level that starts and ends in the same database. *)
  val is_empty : t -> bool

  (** Fold over the new properties added in this level.

      More precisely, and provided the database has been properly normalized, if
      there is a property [P(x)] (where [x] is canonical at the end of the
      level) that holds at the end of the level but did not hold at the start of
      the level, then [fold_properties] is guaranteed to fold over exactly one
      instance of a [P(x) = y] fact.

      {b Note}: This includes not only properties that were effectively added
      between the start and the end of the level, but also properties that held
      for an alias of [x] at the start of the level, if this alias has been
      demoted since the start of the level. *)
  val fold_properties :
    (Function.t -> Name.t -> Simple.t -> coercion:Coercion.t -> 'a -> 'a) ->
    t ->
    'a ->
    'a

  (** Fold over the properties that {b became constant} in this level.

      More precisely, and provided the database has been properly normalized, if
      there is a property [P(x)] whose value is constant at the end of the level
      but it either did not hold or was not constant at the start of the level,
      then [fold_constant_properties] is guaranteed to fold over {b at least
      one} instance of a [P(x') = c] fact where the canonical of [x'] at the
      end of the level is [x]. *)
  val fold_constant_properties :
    (Function.t -> Name.t -> Reg_width_const.t -> 'a -> 'a) -> t -> 'a -> 'a

  (** Returns the set of extensions that were activated in this level, i.e.
      extensions that are active at the end of the level but not at the start of
      the level. *)
  val activated_extensions : t -> Extension_id.Set.t
end
