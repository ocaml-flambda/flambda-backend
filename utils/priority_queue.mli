(** Max priority queue  *)

[@@@ocaml.warning "+a-40-41-42"]

module type Order = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module type Priority_queue = sig
  type priority

  type 'a t

  type 'a element =
    { priority : priority;
      data : 'a
    }

  val make : initial_capacity:int -> 'a t

  val is_empty : 'a t -> bool

  val size : 'a t -> int

  val add : 'a t -> priority:priority -> data:'a -> unit

  val get : 'a t -> 'a element

  val remove : 'a t -> unit

  val get_and_remove : 'a t -> 'a element

  val iter : 'a t -> f:('a element -> unit) -> unit
end

module Make (Priority : Order) : Priority_queue with type priority = Priority.t
