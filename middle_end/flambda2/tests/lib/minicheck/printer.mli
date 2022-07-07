type 'a t = Format.formatter -> 'a -> unit

val opaque : _ t

val opaque_as : string -> _ t

val bool : bool t

val int : int t

val option : 'a t -> 'a option t

val unit : unit t

val pair : 'a t -> 'b t -> ('a * 'b) t

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val list : 'a t -> 'a list t

val fn : ('a -> 'b) t

val function_ : 'b t -> ('a, 'b) Function.t t

module T : sig
  type nonrec 'a t = 'a t
end

val tuple : ('a, 'b) Tuple.Of(T).t -> ('a, 'b) Tuple.t t
