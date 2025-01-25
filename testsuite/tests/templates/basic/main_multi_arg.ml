open Import_multi_arg

module Category_utils_of_semigroup_and_lists =
  Category_utils
    (Category)
    (Product_category
       (Category)(Category_of_monoid(Monoid)(Monoid_of_semigroup))
       (Category_b)(Category_b_of_category
                      (Category)(Category_of_monoid(Monoid)(List_monoid))))
    [@jane.non_erasable.instances]

let concat_chain_semi_and_lists semi_and_lists =
  Category_utils_of_semigroup_and_lists.concat semi_and_lists
  |> Category_of_semigroup_and_lists.to_pair
