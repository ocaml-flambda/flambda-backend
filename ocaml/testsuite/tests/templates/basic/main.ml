module Monoid_utils_of_string =
  Monoid_utils(Monoid)(Monoid_of_semigroup(Semigroup)(String_semigroup))
  [@jane.non_erasable.instances]

module Monoid_utils_of_semigroup =
  Monoid_utils(Monoid)(Monoid_of_semigroup) [@jane.non_erasable.instances]

module Category_utils_of_semigroup =
  Category_utils(Category)(Category_of_monoid(Monoid)(Monoid_of_semigroup))
    [@jane.non_erasable.instances]

module Category_utils_of_list =
  Category_utils(Category)(Category_of_monoid(Monoid)(List_monoid))
    [@jane.non_erasable.instances]

module Category_of_list_monoid =
  Category_of_monoid(Monoid)(List_monoid)
    [@jane.non_erasable.instances]

let concat_string_options = Monoid_utils_of_string.concat

let concat_semi : Monoid_utils_of_semigroup.ts -> Monoid_of_semigroup.t =
  Monoid_utils_of_semigroup.concat

let concat_chain_semi = Category_utils_of_semigroup.concat

let append3_semi a b c = Category_utils_of_semigroup.concat [ a; b; c ]

let concat_lists = List.concat

let concat_chain_lists = Category_utils_of_list.concat

let append3_lists a b c = concat_chain_lists [ a; b; c ]
