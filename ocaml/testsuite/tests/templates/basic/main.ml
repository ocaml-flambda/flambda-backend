module Monoid_utils_of_semigroup =
  [%extension.instances](Monoid_utils(Monoid)(Monoid_of_semigroup))

module Category_utils_of_semigroup =
  [%extension.instances]
    (Category_utils(Category)(Category_of_monoid(Monoid)(Monoid_of_semigroup)))

let concat : Monoid_utils_of_semigroup.ts -> Monoid_of_semigroup.t =
  Monoid_utils_of_semigroup.concat

let concat_chain = Category_utils_of_semigroup.concat
