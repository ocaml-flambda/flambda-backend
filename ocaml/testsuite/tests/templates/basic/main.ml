module Monoid_utils_of_semigroup =
  Monoid_utils(Monoid)(Monoid_of_semigroup) [@jane.non_erasable.instances]

module Category_utils_of_semigroup =
  Category_utils(Category)(Category_of_monoid(Monoid)(Monoid_of_semigroup))
    [@jane.non_erasable.instances]

let concat : Monoid_utils_of_semigroup.ts -> Monoid_of_semigroup.t =
  Monoid_utils_of_semigroup.concat

let concat_chain = Category_utils_of_semigroup.concat
