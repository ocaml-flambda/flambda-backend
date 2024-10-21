type 'a t = { mutable v : 'a; }
val make : 'a -> 'a t @@ portable
val get : 'a t -> 'a @@ portable
val set : 'a t -> 'a -> unit @@ portable
val exchange : 'a t -> 'a -> 'a @@ portable
val compare_and_set : 'a t -> 'a -> 'a -> bool @@ portable
val fetch_and_add : int t -> int -> int @@ portable
val incr : int t -> unit @@ portable
val decr : int t -> unit @@ portable
