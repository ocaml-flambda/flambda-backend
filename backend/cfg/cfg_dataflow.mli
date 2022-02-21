[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR-soon xclerc for xclerc: try and unify Forward_domain/Backward_domain,
   Forward_transfer/Backward_transfer, and Forward_S/Backward_S. *)

module type Forward_domain = sig
  type t

  val top : t

  val bot : t

  val compare : t -> t -> int

  val join : t -> t -> t

  val to_string : t -> string
end

module type Forward_transfer = sig
  type domain

  type t =
    { normal : domain;
      exceptional : domain
    }

  val basic : domain -> Cfg.basic Cfg.instruction -> t

  val terminator : domain -> Cfg.terminator Cfg.instruction -> t
end

module type Forward_S = sig
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

module Forward (D : Forward_domain) (_ : Forward_transfer with type domain = D.t) :
  Forward_S with type domain = D.t

module type Backward_domain = sig
  type t

  val bot : t

  val compare : t -> t -> int

  val join : t -> t -> t

  val less_equal : t -> t -> bool

  val to_string : t -> string
end

module type Backward_transfer = sig
  type domain

  val basic : domain -> exn:domain -> Cfg.basic Cfg.instruction -> domain

  val terminator :
    domain -> exn:domain -> Cfg.terminator Cfg.instruction -> domain

  val exception_ : domain -> domain
end

module type Backward_S = sig
  type domain

  type map = domain Label.Tbl.t

  val run :
    Cfg.t -> ?max_iteration:int -> init:domain -> unit -> (map, map) Result.t
end

module Backward (D : Backward_domain) (_ : Backward_transfer with type domain = D.t) :
  Backward_S with type domain = D.t
