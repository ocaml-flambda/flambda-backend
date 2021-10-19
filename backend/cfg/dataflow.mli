[@@@ocaml.warning "+a-4-30-40-41-42"]

module type Domain = sig
  type t

  val top : t

  val bot : t

  val compare : t -> t -> int

  val join : t -> t -> t

  val to_string : t -> string
end

module type Transfer = sig
  type domain

  type t =
    { normal : domain;
      exceptional : domain
    }

  val basic : domain -> Cfg.basic Cfg.instruction -> t

  val terminator : domain -> Cfg.terminator Cfg.instruction -> t
end

module type S = sig
  type domain

  type init =
    { value : domain;
      in_work_set : bool
    }

  type map = domain Label.Tbl.t

  val run :
    Cfg.t ->
    ?max_iteration:int ->
    init:(Cfg.basic_block -> init) ->
    unit ->
    (map, map) Result.t
end

module Forward (D : Domain) (_ : Transfer with type domain = D.t) :
  S with type domain = D.t

(* CR xclerc for xclerc: move to another module *)
val run_dead_code : Cfg_with_layout.t -> unit
