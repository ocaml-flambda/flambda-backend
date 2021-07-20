(* The issue with this example is that we want to remove the second array
   bounds check thanks to CSE, but this involves doing two rounds at
   some point. So this should only work with -flambda-cse-depth of at least 2
   (the default is 2) *)

external ( - ) : int -> int -> int = "%subint"
external ( = ) : 'a -> 'a -> bool = "%equal"
external array_get: 'a array -> int -> 'a = "%array_safe_get"
external array_set: 'a array -> int -> 'a -> unit = "%array_safe_set"

let f table table c min t =
 assert (array_get table (c - min) = None);
 array_set table (c - min) (Some t)
