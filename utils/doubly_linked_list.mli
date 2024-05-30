type 'a cell

val insert_and_return_before : 'a cell -> 'a -> 'a cell

val insert_before : 'a cell -> 'a -> unit

val insert_and_return_after : 'a cell -> 'a -> 'a cell

val insert_after : 'a cell -> 'a -> unit

val value : 'a cell -> 'a

val set_value : 'a cell -> 'a -> unit

val prev : 'a cell -> 'a cell option

val next : 'a cell -> 'a cell option

type 'a t

val make_empty : unit -> _ t

val make_single : 'a -> 'a t

val of_list : 'a list -> 'a t

val clear : 'a t -> unit

val hd : 'a t -> 'a option

val hd_cell : 'a t -> 'a cell option

val last : 'a t -> 'a option

val last_cell : 'a t -> 'a cell option

val add_begin : 'a t -> 'a -> unit

val add_end : 'a t -> 'a -> unit

val is_empty : 'a t -> bool

val length : 'a t -> int

val remove_first : 'a t -> f:('a -> bool) -> unit

val delete_before : 'a cell -> unit

val delete_after : 'a cell -> unit

val delete_curr : 'a cell -> unit

val filter_left : 'a t -> f:('a -> bool) -> unit

val filter_right : 'a t -> f:('a -> bool) -> unit

val iter : 'a t -> f:('a -> unit) -> unit

val iteri : 'a t -> f:(int -> 'a -> unit) -> unit

val iter_cell : 'a t -> f:('a cell -> unit) -> unit

val iter_right_cell : 'a t -> f:('a cell -> unit) -> unit

val iter2 : 'a t -> 'a t -> f:('a -> 'a -> unit) -> unit

val fold_left : 'a t -> f:('b -> 'a -> 'b) -> init:'b -> 'b

val fold_right : 'a t -> f:('a -> 'b -> 'b) -> init:'b -> 'b

val find_cell_opt : 'a t -> f:('a -> bool) -> 'a cell option

val find_opt : 'a t -> f:('a -> bool) -> 'a option

val exists : 'a t -> f:('a -> bool) -> bool

val for_all : 'a t -> f:('a -> bool) -> bool

val to_list : 'a t -> 'a list

(* Adds all of the elements of `from` to `to_`, and clears `from`. *)
val transfer : to_:'a t -> from:'a t -> unit -> unit
