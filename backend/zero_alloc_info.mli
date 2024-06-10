(** Function summaries computed by zero_alloc analysis
    and encoded as integers for storing in .cmx files. *)

type value = int

type t

val create : unit -> t

val reset : t -> unit

(** [merge_checks t ~into] modifies [into] by adding information from [t]. *)
val merge : t -> into:t -> unit

(** [get_value t fun_name] returns None if [fun_name] is not associated
    with any value. *)
val get_value : t -> string -> value option

val set_value : t -> string -> value -> unit

module Raw : sig
  type t
end

val to_raw : t -> Raw.t

val of_raw : Raw.t -> t

val print : t -> unit
