[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR-soon xclerc for xclerc: try and unify Forward_transfer/Backward_transfer,
   and Forward_S/Backward_S. *)

module type Domain_S = sig
  (* The domain is a join-semilattice with a lowest element. To ensure
     termination additionally all ascending chains have to be bounded. *)
  type t

  (** Identity element of the [join] operation. From definition this is also the
      lowest element in the domain. *)
  val bot : t

  (** Join operator of the join-semilattice. This operation has be associative,
      commutative and idempotent. *)
  val join : t -> t -> t

  (** Operator defined as ([less_equal x y] iff [equal (join x y) y]). Is
      separate from [join] for efficiency. *)
  val less_equal : t -> t -> bool
end

module type Forward_transfer = sig
  type domain

  type context

  type image =
    { normal : domain;
      exceptional : domain
    }

  val basic : domain -> Cfg.basic Cfg.instruction -> context -> domain

  val terminator : domain -> Cfg.terminator Cfg.instruction -> context -> image
end

module type Forward_S = sig
  type domain

  type context

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
    Cfg.t ->
    ?max_iteration:int ->
    init:domain ->
    context ->
    (domain Label.Tbl.t, unit) result
end

module Forward (D : Domain_S) (T : Forward_transfer with type domain = D.t) :
  Forward_S with type domain = D.t and type context = T.context

module type Backward_transfer = sig
  type domain

  type error

  type context

  val basic :
    domain -> Cfg.basic Cfg.instruction -> context -> (domain, error) result

  val terminator :
    domain ->
    exn:domain ->
    Cfg.terminator Cfg.instruction ->
    context ->
    (domain, error) result

  val exception_ : domain -> context -> (domain, error) result
end

module Instr : Identifiable.S with type t = int

module Dataflow_result : sig
  type ('a, 'e) t =
    | Ok of 'a
    | Aborted of 'a * 'e
    | Max_iterations_reached
end

module type Backward_S = sig
  type domain

  type error

  type context

  type _ map =
    | Block : domain Label.Tbl.t map
    | Instr : domain Instr.Tbl.t map
    | Both : (domain Instr.Tbl.t * domain Label.Tbl.t) map

  val run :
    Cfg.t ->
    ?max_iteration:int ->
    ?exnescape:domain ->
    init:domain ->
    map:'a map ->
    context ->
    ('a, error) Dataflow_result.t
end

module Backward (D : Domain_S) (T : Backward_transfer with type domain = D.t) :
  Backward_S
    with type domain = D.t
     and type error = T.error
     and type context = T.context
