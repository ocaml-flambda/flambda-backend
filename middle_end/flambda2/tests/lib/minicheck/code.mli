type (_, _) t =
  | Identity : ('a, 'a) t
  | Const : 'b -> ('a, 'b) t
  | Fun : ('a -> 'b) -> ('a, 'b) t

val call : ('a, 'b) t -> 'a -> 'b
