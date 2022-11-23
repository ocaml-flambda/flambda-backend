type (_, _) t =
  | Identity : ('a, 'a) t
  | Const : 'b -> ('a, 'b) t
  | Fun : ('a -> 'b) -> ('a, 'b) t

let call : type a b. (a, b) t -> a -> b =
 fun f a -> match f with Identity -> a | Const b -> b | Fun f -> f a
