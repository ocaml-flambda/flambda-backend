open Import

val concat : Semigroup.t option list -> Monoid_of_semigroup.t

val concat_chain : (unit, unit) Chain_of_semigroup.t -> Monoid_of_semigroup.t
