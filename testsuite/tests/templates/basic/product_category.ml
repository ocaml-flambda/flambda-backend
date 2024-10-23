type (_, _) t =
  | Pair
       : ('a1, 'a2) Category.t * ('b1, 'b2) Category_b.t
      -> ('a1 * 'b1, 'a2 * 'b2) t
  | Id : ('a, 'a) t

let id = Id
let compose (type a b c) ~(first : (a, b) t) ~(second : (b, c) t) : (a, c) t =
  match first, second with
  | Id, _ -> second
  | _, Id -> first
  | Pair (first_a, first_b), Pair (second_a, second_b) ->
    Pair
      ( Category.compose ~first:first_a ~second:second_a,
        Category_b.compose ~first:first_b ~second:second_b )

let to_pair (type a1 a2 b1 b2) (t : (a1 * b1, a2 * b2) t)
      : (a1, a2) Category.t * (b1, b2) Category_b.t =
  match t with
  | Id -> Category.id, Category_b.id
  | Pair (a, b) -> a, b
