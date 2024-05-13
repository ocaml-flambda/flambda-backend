(** Function summaries computed by zero_alloc analysis
    and encoded as integers for storing in .cmx files. *)

type value = int option

type t

val create : unit -> t

val reset : t -> unit

(** [merge_checks c ~into] modifies [into] by adding
           information from [src]. *)
val merge : t -> into:t -> unit

(** [get_value t] returns None if checks are not enabled  *)
val get_value : t -> string -> value option

val set_value : t -> string -> value -> unit

module Raw : sig
  type t

  val print : t -> unit
end

val to_raw : t -> Raw.t

val of_raw : Raw.t -> t
