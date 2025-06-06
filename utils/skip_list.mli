[@@@ocaml.warning "+a-30-40-41-42"]

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type T = sig
  type elem

  type cell

  val value : cell -> elem

  val next : cell -> cell option

  val cut : cell -> unit

  val delete_curr : cell -> unit

  type t

  val make_empty : max_skip_level:int -> skip_factor:float -> unit -> t

  val length : t -> int

  val clear : t -> unit

  val hd_cell : t -> cell option

  val insert : t -> elem -> unit

  val iter : t -> f:(elem -> unit) -> unit

  val fold_left : t -> f:('a -> elem -> 'a) -> init:'a -> 'a

  val map : t -> f:(elem -> elem) -> t

  val exists : t -> f:(elem -> bool) -> bool

  val to_list : t -> elem list

  val print_for_debug : t -> f:(elem -> string) -> unit

  val invariant : t -> unit
end

module Make (OT : OrderedType) : T with type elem = OT.t
