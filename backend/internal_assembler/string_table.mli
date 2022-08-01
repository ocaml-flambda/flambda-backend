type t

val create : unit -> t

val add_string : t -> string -> unit

val current_length : t -> int

val write : t -> int64 -> Owee.Owee_buf.t -> unit
