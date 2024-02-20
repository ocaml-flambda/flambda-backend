type ts = Monoid.t list

let concat = List.fold_left Monoid.append Monoid.empty
