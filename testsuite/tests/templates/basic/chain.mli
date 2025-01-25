type (_, _) t =
  | [] : ('a, 'a) t
  | (::) : ('a, 'b) Category.t * ('b, 'c) t -> ('a, 'c) t
