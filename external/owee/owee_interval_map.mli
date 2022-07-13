type 'a interval = {
  lbound: int;
  rbound: int;
  value: 'a;
}

val interval : int64 -> int64 -> 'a -> 'a interval

type 'a t

(** {4 Constructors} *)

(** [create intervals_list] : interval tree of all intervals in the list *)
val create : int -> f:(int -> 'a interval) -> 'a t

(** {4 Query} *)

(** [query tree q] : list of intervals in the tree [t] containing
    the Int64.t [q] *)
val query : 'a t -> Int64.t -> 'a interval list

(** [iter tree ~f] calls applies [f] to each interval that has been added to
    [tree].  Traversal order is not specified.  *)
val iter : 'a t -> f:('a interval -> unit) -> unit
