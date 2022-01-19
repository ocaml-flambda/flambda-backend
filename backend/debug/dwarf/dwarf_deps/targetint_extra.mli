include module type of Targetint

(** The width of a target integer in bytes, expressed as a value of type [t]. *)
val size_in_bytes_as_targetint : t

(** Convert the given target integer to an unsigned 64-bit integer. *)
val to_uint64_exn : t -> Numbers_extra.Uint64.t
