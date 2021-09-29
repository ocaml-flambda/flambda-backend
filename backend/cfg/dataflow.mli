module type Domain = sig
  type t

  val equal : t -> t -> bool

  val join : t -> t -> t

  val to_string : t -> string
end

module type Transfer = sig
  type domain

  val basic : domain -> Cfg.basic Cfg.instruction -> domain

  val terminator : domain -> Cfg.terminator Cfg.instruction -> domain

  val exception_ : domain -> domain
end

module type S = sig
  type domain

  type value =
    { before : domain;
      after : domain option;
      exception_ : domain option
    }

  val run :
    Cfg.t ->
    ?max_iteration:int ->
    init:(Cfg.basic_block -> domain) ->
    unit ->
    value Label.Tbl.t
end

module Forward (D : Domain) (_ : Transfer with type domain = D.t) :
  S with type domain = D.t

(* CR xclerc for xclerc: move to another module *)
val run_dead_code : Cfg_with_layout.t -> unit
