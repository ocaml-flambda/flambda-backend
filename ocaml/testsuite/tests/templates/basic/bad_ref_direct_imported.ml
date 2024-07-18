(* [Monoid] is not a parameter of this, but it _is_ imported because it's used
   as a parameter *)

module Monoid_utils_of_semigroup =
  Monoid_utils(Monoid)(Monoid_of_semigroup) [@jane.non_erasable.instances]

let empty = Monoid.empty
