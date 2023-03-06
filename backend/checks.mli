(** Symbols of function that pass certain checks for special properties. *)

type value =
  { nor : bool option;
    exn : bool option;
    div : bool option
  }

type t

val create : unit -> t

val reset : t -> unit

(** [merge_checks c ~into] modifies [into] by adding
           information from [src]. *)
val merge : t -> into:t -> unit

val get_value : t -> string -> value

val set_value : t -> string -> value -> unit

module Raw : sig
  type t

  val print : t -> unit
end

val to_raw : t -> Raw.t

val of_raw : Raw.t -> t
