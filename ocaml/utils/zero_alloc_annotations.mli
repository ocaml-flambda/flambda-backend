type t = Check_default | Check_all | Check_opt_only | No_check
val all : t list
val to_string : t -> string
val of_string : string -> t option
val equal : t -> t -> bool
val doc : string
