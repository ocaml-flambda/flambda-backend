include module type of struct include Targetint end

val size_in_bytes_as_targetint : t
(** The width of a target integer in bytes, expressed as a value of type [t]. *)

val to_uint64_exn : t -> Numbers.Uint64.t
(** Convert the given target integer to an unsigned 64-bit integer. *)