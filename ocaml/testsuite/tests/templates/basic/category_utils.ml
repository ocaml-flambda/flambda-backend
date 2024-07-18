let rec concat : type a b. (a, b) Chain.t -> (a, b) Category.t =
 fun chain ->
  match chain with
  | [] -> Category.id
  | a_to_b :: b_to_c -> Category.compose ~first:a_to_b ~second:(concat b_to_c)
