(** Abstract domain used in static analysis for checking annotations such as @zero_alloc.
    See [backend/checkmach] for details of the analysis.
    See [lambda/assume_info.ml] for details about the translation of
    user-provided annotations to abstract values in this domain.
*)
module type WS = sig
  type t

  val join : t -> t -> t

  val meet : t -> t -> t

  val lessequal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module Make (Witnesses : WS) : sig
  (** Abstract value for each component of the domain. *)
  module V : sig
    type t =
      | Top of Witnesses.t  (** Property may not hold on some paths. *)
      | Safe  (** Property holds on all paths.  *)
      | Bot  (** Not reachable. *)

    val lessequal : t -> t -> bool

    val join : t -> t -> t

    val meet : t -> t -> t

    val is_not_safe : t -> bool

    val print : witnesses:bool -> Format.formatter -> t -> unit
  end

  (** Abstract value associated with each program location in a function. *)
  module Value : sig
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
end
