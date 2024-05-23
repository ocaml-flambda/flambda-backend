[@@@ocaml.warning "+a-4-30-40-41-42"]

type value =
  | No_checks
  | Check_as_first_instruction of
      { size_in_bytes : int;
        includes : string list (* CR-soon xclerc for xclerc: for debug/test *)
      }
  | Check_moved_down

type t

val create : unit -> t

val reset : t -> unit

val merge : t -> into:t -> unit

val get_value : t -> string -> value option

val set_value : t -> string -> value -> unit

module Raw : sig
  type t

  val print : t -> unit
end

val to_raw : t -> Raw.t

val of_raw : Raw.t -> t
