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

  type map = domain Label.Tbl.t

  (** Perform the dataflow analysis on the passed CFG, returning [OK _] if a
      fix-point has been reached and [Error _] otherwise, where the nested value
      is a partial map from labels to the domain values at the start of the
      corresponding blocks. If [Error _] is returned then the contents of the
      map is not guaranteed to be sound.

      A fix-point is not reached if there is still pending work after
      [max_iteration] (defaulting to [max_int]) have been executed, and
      iteration being the processing of one element from the working set. The
      [init] value is the initial value of entry points. *)
  val run :
    Cfg.t -> ?max_iteration:int -> ?init:domain -> unit -> (map, map) Result.t
end

module Forward (D : Domain) (_ : Transfer with type domain = D.t) :
  S with type domain = D.t
