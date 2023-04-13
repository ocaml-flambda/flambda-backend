[@@@ocaml.warning "+a-4-30-40-41-42"]

module Description : sig
  type t

  val create : Cfg_with_layout.t -> t option
end

module Error : sig
  type t

  val print : Format.formatter -> t -> unit
end

val test :
  Description.t -> Cfg_with_layout.t -> (Cfg_with_layout.t, Error.t) Result.t

val run : Description.t option -> Cfg_with_layout.t -> Cfg_with_layout.t
