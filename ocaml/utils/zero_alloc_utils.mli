(** Abstract domain used in static analysis for checking annotations such as @zero_alloc.
    See [backend/checkmach] for details.
*)
module type WS = sig
  type t

  val join : t -> t -> t

  val meet : t -> t -> t

  val print : Format.formatter -> t -> unit
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

    val normal_return : t

    val exn_escape : t

    val diverges : t

    val safe : t

    val relaxed : Witnesses.t -> t

    val print : witnesses:bool -> Format.formatter -> t -> unit
  end
end
