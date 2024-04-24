(** Abstract domain used in static analysis for checking annotations such as @zero_alloc.
    See [backend/checkmach] for details of the analysis.
    See this module's .ml file for details about the translation of
    user-provided annotations to abstract values in this domain.
*)
module type WS = sig
  type t

  val empty : t

  val join : t -> t -> t

  val meet : t -> t -> t

  val lessequal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module type Component = sig
  (** Abstract value for each component of the domain. *)
  type t

  type witnesses

  (** Property may not hold on some paths. *)
  val top : witnesses -> t

  (** Property holds on all paths.  *)
  val safe : t

  (** Not reachable. *)
  val bot : t

  (** Order of the abstract domain  *)
  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  (**  Use [compare] for structural comparison of terms, for example to store them in a
       set. Use [lessequal] for checking fixed point of the abstract domain.  *)
  val compare : t -> t -> int

  val print : witnesses:bool -> Format.formatter -> t -> unit
end

module Make_component (Witnesses : WS) : sig
  type t =
    | Top of Witnesses.t
    | Safe
    | Bot

  include Component with type witnesses := Witnesses.t and type t := t
end

module Make_value
    (Witnesses : WS)
    (V : Component with type witnesses := Witnesses.t) : sig
  (** Abstract value associated with each program location in a function. *)
  type t =
    { nor : V.t;
          (** Property about
          all paths from this program location that may reach a Normal Return  *)
      exn : V.t;
          (** Property about all paths from this program point that may reach a Return with
          Exception *)
      div : V.t
          (** Property about all paths from this program point that may diverge.  *)
    }

  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val top : Witnesses.t -> t

  val bot : t

  (** [normal_return] means property holds on paths to normal return, exceptional return
        is not reachable and execution will not diverge.  *)
  val normal_return : t

  (** [exn_escape] means the property holds on paths to exceptional return, normal
        return is not reachable and execution will not diverge.  *)
  val exn_escape : t

  (** [diverges] means the execution may diverge without violating the property, but
        normal and exceptional return are not reachable (i.e., [div] is Safe, `nor` and
        `exn` are Bot). *)
  val diverges : t

  (** [safe] means the property holds on all paths (i.e., all three components are set
        to Safe).  *)
  val safe : t

  (** [relaxed] means the property holds on paths that lead to normal returns only
        (i.e., [nor] component is Safe, others are Top.  *)
  val relaxed : Witnesses.t -> t

  (** Constructs a value from a user annotation. The witness will be empty. *)
  val of_annotation : strict:bool -> never_returns_normally:bool -> t

  val print : witnesses:bool -> Format.formatter -> t -> unit

  (** Use [compare] for structural comparison of terms, for example
        to store them in a set. Use [lessequal]
        for checking fixed point of the abstract domain.

        [compare] must be consistent with [lessthan] order of the abstract domain,
        i.e., if [compare t1 t2 <= 0] then [lessequal v1 v2 = true].

        [compare] may distinguish terms that are equivalent
        w.r.t. the abstraction, i.e., [lessequal v1 v2 = true]
        does not always imply [compare t1 t2 <= 0].
        In particular, [compare] distinguishes between two "Top" values
        with different witnesses. *)
  val compare : t -> t -> int
end

(** The [Assume_info] module contains an instantiation of the abstract domain
   with trivial witnesses.  It is used to propagate assume annotations from the
   front-end to the backend.  The backend contains a different instantiation of
   the abstract domain where the witnesses contain actual information about
   allocations - see [Checkmach].  *)
module Assume_info : sig
  type t

  val none : t

  val create : strict:bool -> never_returns_normally:bool -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val to_string : t -> string

  val print : Format.formatter -> t -> unit

  val is_none : t -> bool

  module Witnesses : sig
    type t = unit

    val empty : t

    val join : t -> t -> t

    val lessequal : t -> t -> bool

    val meet : t -> t -> t

    val print : Format.formatter -> t -> unit

    val compare : t -> t -> int
  end

  module V : module type of Make_component (Witnesses)

  module Value : module type of Make_value (Witnesses) (V)

  val get_value : t -> Value.t option
end
