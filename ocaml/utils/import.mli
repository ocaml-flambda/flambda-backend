(** The bare name of an imported module, without a pack prefix or instance
    arguments. Corresponds to exactly one .cmi file. *)
type t

include Identifiable.S with type t := t

val of_string : string -> t

val of_head_of_global_name : Global.Name.t -> t

val to_string : t -> string

val dummy : t

val predef_exn : t
