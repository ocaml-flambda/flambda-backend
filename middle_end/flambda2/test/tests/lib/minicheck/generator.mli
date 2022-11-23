[@@@ocaml.warning "+a-4-30-40-41-42"]

type 'a t = Splittable_random.t -> 'a

val generate : 'a t -> Splittable_random.t -> 'a

val bool : bool t

val int : int t

(** Integer between zero (inclusive) and [less_than] (exclusive). Not perfectly
    uniform, but very close if [less_than] << [max_int]. *)
val small_nat : less_than:int -> int t

(** Integer whose size in bits is uniformly random. So called because it
    approximates a log-uniform distribution. *)
val log_int : int t

val option : 'a t -> 'a option t

val unit : unit t

val pair : 'a t -> 'b t -> ('a * 'b) t

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val const : 'a -> 'a t

val one_of : 'a list -> 'a t

val list : 'a t -> length:int -> 'a list t

val fn : ?hash_arg:('a -> int) -> 'b t -> ('a -> 'b) t

val fn2 : ?hash_args:('a * 'b -> int) -> 'c t -> ('a -> 'b -> 'c) t

val fn3 : ?hash_args:('a * 'b * 'c -> int) -> 'd t -> ('a -> 'b -> 'c -> 'd) t

val function_ : ?hash_arg:('a -> int) -> 'b t -> ('a, 'b) Function.t t

val function_w_id : ?hash_arg:('a -> int) -> 'a t -> ('a, 'a) Function.t t

(** Generate a value from one of the given generators, each having the given
    relative weight. *)
val choose : (int * 'a t) list -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val bind : 'a t -> f:('a -> 'b t) -> 'b t

(** Keep generating a value until one passes the filter *)
val filter : ?max_attempts:int -> 'a t -> f:('a -> bool) -> 'a t

module Let_syntax : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

module T : sig
  type nonrec 'a t = 'a t
end

val tuple : ('a, 'b) Tuple.Of(T).t -> ('a, 'b) Tuple.t t
