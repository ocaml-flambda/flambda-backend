module Chain_of_semigroup =
  Chain(Category)(Category_of_monoid(Monoid)(Monoid_of_semigroup))
    [@jane.non_erasable.instances]

module Chain_of_lists =
  Chain(Category)(Category_of_monoid(Monoid)(List_monoid))
    [@jane.non_erasable.instances]
