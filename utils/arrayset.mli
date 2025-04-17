(** Set implemented as an array.  *)

[@@@ocaml.warning "+a-40-41-42"]

(* CR-soon gyorsh: check whether the dynamic array module from the stdlib can be
   used *)

module type S = sig
  type e

  type t

  val make : original_capacity:int -> t

  val clear : t -> unit

  val is_empty : t -> bool

  val choose_and_remove : t -> e option

  val add : t -> e -> unit

  val remove : t -> e -> unit

  val iter : t -> f:(e -> unit) -> unit

  val fold : t -> f:('a -> e -> 'a) -> init:'a -> 'a

  val to_list : t -> e list
end

module type OrderedTypeWithDummy = sig
  include Set.OrderedType

  val dummy : t (* note: does not 0-compare to any "interesting" value *)
end

module Make (T : OrderedTypeWithDummy) : S with type e = T.t
