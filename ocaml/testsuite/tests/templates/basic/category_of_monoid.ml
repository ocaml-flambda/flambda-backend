type ('a, 'b) t = Monoid.t

let id = Monoid.empty
let compose ~first ~second = Monoid.append first second

let as_unit t = t
