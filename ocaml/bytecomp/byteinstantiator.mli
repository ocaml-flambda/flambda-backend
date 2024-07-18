val instantiate : src:string -> args:string list -> string -> unit

type error =
  | Not_an_object_file of Misc.filepath

exception Error of error

val report_error : Format.formatter -> error -> unit
