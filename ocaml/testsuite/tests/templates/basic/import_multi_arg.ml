module Category_of_semigroup_and_lists =
  Product_category
    (Category)(Category_of_monoid(Monoid)(Monoid_of_semigroup))
    (Category_b)(Category_b_of_category
                   (Category)(Category_of_monoid(Monoid)(List_monoid)))
    [@jane.non_erasable.instances]

module Chain_of_semigroup_and_lists =
  Chain
    (Category)
    (Product_category
       (Category)(Category_of_monoid(Monoid)(Monoid_of_semigroup))
       (Category_b)(Category_b_of_category
                      (Category)(Category_of_monoid(Monoid)(List_monoid))))
    [@jane.non_erasable.instances]
